// *******************************************************************
// ***** Minimal UART Tiny Emulator by Carsten Herting 20.7.2025 *****
// *******************************************************************

// sudo apt install picocom
// picocom -b 125000 --databits 7 --parity n --stopbits 2 --flow n --imap lfcrlf --omap crlf /dev/ttyUSB0
// type 'receive' and upload file from another terminal: ./asm.exe mandel.asm | ./send.sh

#include "mnemonics.h"
#include "asm.h"

#undef TERMINAL_TO_FILE

//                          <------><------><------>
#define CTRL_INVERT_MASK  0b111111100111111111111001 // 0: control signal is active HIGH, 1: control signal is active LOW

#define FF                0b000000000000000000000000 // passive bus 0xff

#define EO                0b000000000000000000000001 // LSB
#define EC                0b000000000000000000000010
#define ES                0b000000000000000000000100
#define BI                0b000000000000000000001000
#define BO                0b000000000000000000010000
#define AI                0b000000000000000000100000
#define AO                0b000000000000000001000000
#define NN                0b000000000000000010000000 // unused
#define II                0b000000000000000100000000 // MSB
#define FI                0b000000000000001000000000
#define IC                0b000000000000010000000000
#define CIL               0b000000000000100000000000
#define CIH               0b000000000001000000000000
#define COL               0b000000000010000000000000
#define COH               0b000000000100000000000000
#define CE                0b000000001000000000000000
#define ME                0b000000010000000000000000 // HSB
#define MIL               0b000000100000000000000000
#define MIH               0b000001000000000000000000
#define TI                0b000010000000000000000000
#define TO                0b000100000000000000000000
#define TR                0b001000000000000000000000
#define RI                0b010000000000000000000000
#define RO                0b100000000000000000000000

// flag lines set by ALU and read by flags-Register as "bus" input
#define FLAG_Z						0b0001									   // ~zero
#define FLAG_N						0b0010									   // negative
#define FLAG_C						0b0100									   // carry
#define FLAG_R						0b1000									   // ~data ready

#define TARGET_MENU       0
#define TARGET_SERIAL     1

enum COMPSTATE {RUN, HALT, SINGLESTEP, NEXTINST};
uint32_t gKeyTarget {TARGET_MENU};									 // defines the current keyboard input mode, used by the keyboard callback function
bool gFlashLoaded {false};
std::string gSerialInput;														 // the global strings receive input from the keyboard callback function
std::string gMenuInput;

#include <iostream> // console output
#include <iomanip> // for std::setfill, std::hex,, std::dec
#include <fstream> // file input
#include <cstdint>
#include <cstring>
#include <string>
#include <stdexcept>
#include <memory> // smart pointers
#include <vector> // dynamic arrays
#include <deque>
#include <thread>
#include <algorithm>
#include <utility> // for exchange
#include <unordered_map> // for shader uniform location cache

#include <glad/glad.h> // include OpenGL-Extension-Loader before GLFW
#include <GLFW/glfw3.h> // GLFW: GLFW_VERSION_MAJOR wird definiert -> nimmt Einfluss auf main()
#include <glm/glm.hpp> // OpenGL Math Header Library
#include <glm/gtc/type_ptr.hpp> // für glm::value_ptr

extern "C" const char _binary_shader_glsl_start, _binary_shader_glsl_end; // linked binary resource symbols
extern "C" const char _binary_charset_bin_start, _binary_charset_bin_end;

class Shader
{
public:
  Shader(const char* start, const char* end) // takes pointer to embedded resource data or an identifier "id_..."
  {
    if (!start || !end || start >= end) throw std::runtime_error("Invalid shader source range provided.");
    std::string_view fullShaderCode(start, end-start); // this doesn't copy the string, very lightweight!
    std::string_view vertexShaderCode = ExtractShaderCode(fullShaderCode, "SHADER VERTEX");
    if (vertexShaderCode.empty()) throw std::runtime_error("Vertex shader code section is empty or missing.");
    std::string_view fragmentShaderCode = ExtractShaderCode(fullShaderCode, "SHADER FRAGMENT");
    if (fragmentShaderCode.empty()) throw std::runtime_error("Fragment shader code section is empty or missing.");
    GLuint vshaderid = CompileShader(GL_VERTEX_SHADER, vertexShaderCode);
    if (!vshaderid) throw std::runtime_error("Vertex Shader Compilation Error:\n" + mInfoLog);
    GLuint fshaderid = CompileShader(GL_FRAGMENT_SHADER, fragmentShaderCode);
    if (!fshaderid)
    {
      glDeleteShader(vshaderid);
      throw std::runtime_error("Fragment Shader Compilation Error:\n" + mInfoLog);
    }
    mProgID = LinkProgramDeleteShaders(vshaderid, fshaderid);
    if (!mProgID) throw std::runtime_error("Shader Linking Error\n" + mInfoLog);
  }
  ~Shader() noexcept { if (mProgID) glDeleteProgram(mProgID); mProgID=0; }
  Shader(const Shader& other) = delete; // no shallow copy for elements with allocated resources!
  Shader& operator=(const Shader& other) = delete;
  Shader(Shader&&) = delete;
  Shader& operator=(Shader&&) = delete;
  void Bind() const { glUseProgram(mProgID); }
  static void Unbind() { glUseProgram(0); } // Note: This is a static method as it affects global OpenGL state.
  template<typename T> void SetUniform(std::string_view name, const T& value); // using a template for different datatypes
private:
  std::string_view ExtractShaderCode(std::string_view code, const std::string& marker)
  {
    size_t startPos = code.find(marker);
    if (startPos == std::string_view::npos) throw std::runtime_error(marker + " section not found");
    startPos += marker.size();
    size_t endPos = code.find("SHADER END", startPos);
    if (endPos == std::string_view::npos) throw std::runtime_error(marker + " end marker not found");
    return code.substr(startPos, endPos - startPos);
  }
  GLuint CompileShader(GLenum type, std::string_view code)
  {
    mInfoLog.clear();
    
    GLuint shaderid = glCreateShader(type);
    if (!shaderid) { mInfoLog = "glCreateShader failed\n"; return 0; }

    const GLchar* shaderSrc = code.data();
    GLint shaderLen = static_cast<GLint>(code.size());
    glShaderSource(shaderid, 1, &shaderSrc, &shaderLen);
    glCompileShader(shaderid);
    GLint status; // Container für shader compiler status
    glGetShaderiv(shaderid, GL_COMPILE_STATUS, &status);
    if (status == GL_FALSE)
    {
      GLint len; glGetShaderiv(shaderid, GL_INFO_LOG_LENGTH, &len); // includes the '\0'
      if (len > 1)
      {
        mInfoLog.resize(static_cast<std::size_t>(len));
        glGetShaderInfoLog(shaderid, mInfoLog.size(), nullptr, mInfoLog.data());
        mInfoLog.pop_back(); // drop '\0'
      }
      glDeleteShader(shaderid); // free allocated resources
      return 0;
    }
    return shaderid;
  }
  GLuint LinkProgramDeleteShaders(GLuint vertexshaderid, GLuint fragmentshaderid)
  {
    mInfoLog.clear();

    GLuint progid = glCreateProgram(); // VS + FS = ShaderProg
    if (!progid) { mInfoLog = "glCreateProgram failed\n"; return 0; }

    glAttachShader(progid, vertexshaderid);
    glAttachShader(progid, fragmentshaderid);
    glLinkProgram(progid); // now the attached ShaderObjects are no longer needed and should be detached and than deleted.
    glDetachShader(progid, vertexshaderid); // cleanup
    glDetachShader(progid, fragmentshaderid);
    glDeleteShader(vertexshaderid); // mark for deletion (they are only really deleted after detachment!)
    glDeleteShader(fragmentshaderid); // undoes the effect of glCreateShader()

    GLint status; // Container für shader compiler status
    glGetProgramiv(progid, GL_LINK_STATUS, &status); // Status des Programms abfragen
    if (status == GL_FALSE)
    {
      GLint len; glGetProgramiv(progid, GL_INFO_LOG_LENGTH, &len); // includes the '\0'
      if (len > 1)
      {
        mInfoLog.resize(static_cast<std::size_t>(len));
        glGetProgramInfoLog(progid, mInfoLog.size(), nullptr, mInfoLog.data());
        mInfoLog.pop_back(); // drop '\0'
      }
      glDeleteProgram(progid);
      return 0;
    }
    return progid;
  }
  int GetUniformLocation(std::string_view name) const
  {
    std::string nameStr = std::string(name);
    auto it = mUniLocCache.find(nameStr); if (it != mUniLocCache.end()) return it->second; // Found in cache

    int location = glGetUniformLocation(mProgID, nameStr.c_str()); // erfrage die Location der Variable zum shaderprog ID von der GPU
    if (location == -1) std::cerr << "Shader: Unknown uniform: " << name << std::endl;
    mUniLocCache[nameStr] = location; // speichere die location dieser Variable in der bufptr
    return location; // gib die location dann auch zurück
  }
  mutable std::unordered_map<std::string, int> mUniLocCache; // bereits erfragte Locations speichern
  GLuint mProgID {0};
  std::string mInfoLog; // common storage for info log (compiler error messages outliving helper functions)
};
template<> inline void Shader::SetUniform<int>(std::string_view name, const int& i) { glUniform1i(GetUniformLocation(name), i); }
template<> inline void Shader::SetUniform<glm::vec4>(std::string_view name, const glm::vec4& vec) { glUniform4f(GetUniformLocation(name), vec.x, vec.y, vec.z, vec.w); }
template<> inline void Shader::SetUniform<glm::mat4>(std::string_view name, const glm::mat4& mat) { glUniformMatrix4fv(GetUniformLocation(name), 1, GL_FALSE, glm::value_ptr(mat)); }

class Display
{
public:
  Display(int width, int height)
  : mWidth(width), mHeight(height), mVideoBuffer(width * height, 0), mVertices(6, glm::vec4(0))
  {
    glGenTextures(1, &mTexture);
    glActiveTexture(GL_TEXTURE0); // any texture-related operations (like glBindTexture) will now apply to texture unit 0 (default)

    glGenBuffers(1, &mVBO);
    glGenVertexArrays(1, &mVAO); // generate buffers
    glBindVertexArray(mVAO); // begin recording into VAO...
      glBindBuffer(GL_ARRAY_BUFFER, mVBO); // VAO bezieht die folgenden calls auf diesen VBO
      glBufferData(GL_ARRAY_BUFFER, mVertices.size() * sizeof(glm::vec4), mVertices.data(), GL_STATIC_DRAW);
      glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(glm::vec4), (GLvoid *)(0 * sizeof(GLfloat)));
      glEnableVertexAttribArray(0); // VAO: schalte Attribut "0" ein
      glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(glm::vec4), (GLvoid *)(2 * sizeof(GLfloat))); // VAO: Attribut "0": Size 4 floats pro Attribut
      glEnableVertexAttribArray(1);
    glBindVertexArray(0); // stop recording into VAO...
    glBindBuffer(GL_ARRAY_BUFFER, 0); // unbind the VBO (not recorded)

    float w = width; float h = height;
    mProjMatrix = glm::ortho(0.0f, w, h, 0.0f, -1.0f, 1.0f);

    mShader.Bind();
    mShader.SetUniform("proj", mProjMatrix);
    mShader.SetUniform("drawtexunit", 0); // tell the shader via uniform to sample the texture from unit GL_TEXTURE0

    mVertices[0] = glm::vec4(0.0f,  0.0f,   0.0f, 0.0f); // calculate canvas rect vertices (two triangles)
    mVertices[1] = glm::vec4(w,     0.0f,   1.0f, 0.0f);
    mVertices[2] = glm::vec4(w,     h,      1.0f, 1.0f);

    mVertices[3] = glm::vec4(0.0f,  0.0f,   0.0f, 0.0f);
    mVertices[4] = glm::vec4(w,     h,      1.0f, 1.0f);
    mVertices[5] = glm::vec4(0.0f,  h,      0.0f, 1.0f);

    glBindBuffer(GL_ARRAY_BUFFER, mVBO); // update vertex data
      glBufferSubData(GL_ARRAY_BUFFER, 0, mVertices.size() * sizeof(glm::vec4), mVertices.data());
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glBindTexture(GL_TEXTURE_2D, mTexture);	// all upcoming GL_TEXTURE_2D operations now have effect on only this texture object
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr); // reserve texture memory on GPU
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glBindTexture(GL_TEXTURE_2D, 0);
  }
  ~Display()
  {
    if (mTexture) { glDeleteTextures(1, &mTexture); mTexture = 0; }
    if (mVAO) { glDeleteVertexArrays(1, &mVAO); mVAO = 0; } // vertex array in der GPU löschen
    if (mVBO) { glDeleteBuffers(1, &mVBO); mVBO = 0; } // vertex buffer in der GPU löschen
  }
  Display(const Display& other) = delete; // no shallow copy for elements with allocated resources!
  Display& operator=(const Display& other) = delete;
  Display(Display&&) = delete;
  Display& operator=(Display&&) = delete;
  uint32_t* GetVideoRAMPtr() { return mVideoBuffer.data(); }
  void SetTint(float r, float g, float b, float a) { mTint = glm::vec4(r, g, b, a); }
  void Clear() { memset(mVideoBuffer.data(), 0, mWidth*mHeight*sizeof(uint32_t)); }
  void Render()
  {
    glBindTexture(GL_TEXTURE_2D, mTexture);	// All upcoming GL_TEXTURE_2D operations now have effect on only this texture object
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, mWidth, mHeight, GL_RGBA, GL_UNSIGNED_BYTE, mVideoBuffer.data());	// part of the texture aktualisieren
    mShader.Bind();
    mShader.SetUniform("tint", mTint);
    glBindVertexArray(mVAO);
      glDrawArrays(GL_TRIANGLES, 0, mVertices.size()); // render texture
    glBindVertexArray(0);
    glBindTexture(GL_TEXTURE_2D, 0);
  }
protected:
  Shader mShader {&_binary_shader_glsl_start, &_binary_shader_glsl_end};
  GLuint mTexture {0}, mVAO {0}, mVBO {0};
  int mWidth, mHeight;
  std::vector<uint32_t> mVideoBuffer;
  glm::mat4 mProjMatrix;
  std::vector<glm::vec4> mVertices;
  glm::vec4 mTint {1.0f, 1.0f, 1.0f, 1.0f};
};

class TextScreen : public Display
{
public:
  TextScreen(const uint32_t* charset, int cols, int rows)
  : mCharsetPtr(charset), mCols(cols), mRows(rows), Display(cols<<3, rows<<3) {}
  ~TextScreen() {}
  void Goto(int x, int y) { mXCur = x; mYCur = y; }
	void Move(int dx, int dy) { mXCur += dx; mYCur += dy; }
  void Color(int cindex) { mColorIndex = cindex; }
  void Print(std::string text)
  {
    for (int i=0; i<text.length(); i++)
    {
      if (text[i] == '\n') { mXCur = 0; mYCur++; }
      else
      {
        uint32_t* vptr = mVideoBuffer.data() + (mYCur<<3)*mWidth + (mXCur<<3);
        if (mXCur < mCols && mYCur < mRows) // print only in visible area
        {
          int index = text[i]<<3; // index into charset
          for (int y=0; y<8; y++)
          {
            for (int x=0; x<8; x++) vptr[x] = mCharsetPtr[index + x] & mColors[mColorIndex];
            vptr += mWidth; index += 2048; // goto next row
          } 
        } 
        mXCur++; // advance cursor
      }
    }
  }
  void Print(int x, int y, int cindex, std::string text) { Goto(x, y); Color(cindex); Print(text); }
protected:
  const uint32_t* mCharsetPtr;
  int mCols, mRows;
  int mXCur {0}, mYCur {0}, mColorIndex {0};
  uint32_t mColors[10] { 0xffffffff, 0xff808080, 0xff0000ff, 0xff00ff00, 0xffff0000,   // 0: white, gray, red, green, blue, 
                         0xff00ffff, 0xffff00ff, 0xffffff00, 0xffff8080, 0xff8080ff }; // 5: yellow, magenta, cyan, lt blue, lt red
};

class Terminal : public TextScreen
{
public:
  Terminal(const uint32_t* charset, int cols, int rows) : TextScreen(charset, cols, rows), mCharRAM(cols * rows, 0)
  {
    #ifdef TERMINAL_TO_FILE
      mFile.open("term.txt", std::ofstream::out);
    #endif
  }
  ~Terminal()
  {
    #ifdef TERMINAL_TO_FILE
      mFile.close();
    #endif
  }
  Terminal(const Terminal& other) = delete;									// no shallow copy for elements with allocated resources!
  Terminal& operator=(const Terminal& other) = delete;			// no shallow copy for elements with allocated resources!
  void Render()
  {
    uint8_t* cram = mCharRAM.data(); // pointer into character RAM
    for (int r=0; r<mRows; r++)
    {
      for (int c=0; c<mCols; c++)
      {
        uint32_t* vptr = mVideoBuffer.data() + mWidth*(r<<3) + (c<<3); // pointer into video RAM position
        const uint32_t* cptr = mCharsetPtr + ((*cram++)<<3); // fetch next char from CRAM and point into charset
        for (int y=0; y<8; y++) { memcpy(vptr, cptr, 8*sizeof(uint32_t)); vptr += mWidth; cptr += 256*8; }
      }
    }
    if (mShowCursor) // draw cursor, too
    {
      if (mXCur < mCols && mYCur < mRows)
      {
        uint32_t* vptr = mVideoBuffer.data() + mWidth*(mYCur<<3) + (mXCur<<3); // pointer into video RAM position
        for (int y=0; y<8; y++)
        {
          for (int x=0; x<8; x++) *vptr++ = *vptr | 0xffc0c0c0;
          vptr += mWidth - 8;
        }
      }
    }
    Display::Render(); // call the Render function of the base class
  }
  uint8_t* GetCharRAMPtr() { return mCharRAM.data(); }

  void ProcessChar(uint8_t inbyte) // processes a character (accepts some VT52/GEMDOS ESC sequences, control chars, normal chars)
  {
    #ifdef TERMINAL_TO_FILE
      if (mFile.is_open()) mFile << char(inbyte);
    #endif
    if (mEscBuffer.size() > 5) mEscActive = false;	// lösche Kommandos, die mit 4 bytes Länge nicht bearbeitet werden
    if (inbyte == 27 && mEscActive == false) { mEscActive = true; mEscBuffer = ""; return; }    // start ESC sequence

    if (mEscActive)                           // ES IST BEREITS EINE ESC SEQUENCE AKTIV
    {
      mEscBuffer += inbyte;                   // Zeichen dem ESC buffer hinzufügen
      if (mEscBuffer[0] == '[')
      {
        switch(inbyte)   						         	// Für jede ESC sequence muss geprüft werden, ob damit der Befehl komplett ist
        {
          case 'l':
          {
            if (mEscBuffer.substr(1) == "?25l") mShowCursor = false;
            mEscActive = false;
            break;
          }
          case 'h':
          {
            if (mEscBuffer.substr(1) == "?25h") mShowCursor = true;
            mEscActive = false;
            break;
          }
          case 'A': // move cursor up
          {
            std::string str = mEscBuffer.substr(1, mEscBuffer.size()-2);
            if (str == "") str = "1";
            for(int i=0; i<std::stoi(str); i++) if (mYCur > 0) mYCur--;
            mEscActive = false;
            break;
          }
          case 'B': // move cursor down
          {
            std::string str = mEscBuffer.substr(1, mEscBuffer.size()-2);
            if (str == "") str = "1";
            for(int i=0; i<std::stoi(str); i++) if (mYCur < mRows-1) mYCur++;
            mEscActive = false;
            break;
          }
          case 'C': // move cursor right
          {
            std::string str = mEscBuffer.substr(1, mEscBuffer.size()-2);
            if (str == "") str = "1";
            for(int i=0; i<std::stoi(str); i++) if (mXCur < mCols-1) mXCur++;
            mEscActive = false;
            break;
          }
          case 'D': // move cursor left
          {
            std::string str = mEscBuffer.substr(1, mEscBuffer.size()-2);
            if (str == "") str = "1";
            for(int i=0; i<std::stoi(str); i++) if (mXCur > 0) mXCur--;
            mEscActive = false;
            break;
          }
          case 'G': // move cursor to abs x position
          {
            std::string str = mEscBuffer.substr(1, mEscBuffer.size()-2);
            if (str == "") mXCur = 0;
            else mXCur = std::max(0, std::min(mCols-1, std::stoi(str)-1));
            mEscActive = false;
            break;
          }
          case 'd': // move cursor to abs y position
          {
            std::string str = mEscBuffer.substr(1, mEscBuffer.size()-2);
            if (str == "") mXCur = 0;
            else mYCur = std::max(0, std::min(29, std::stoi(str)-1));
            mEscActive = false;
            break;
          }
          case 'H': // move cursor to upper left corner
            mXCur = mYCur = 0;
            mEscActive = false;
            break;
          case 'J': // clear video ram from cursor onwards
          {
            int startindex = mXCur + mYCur * mCols;
            for(int i=startindex; i<mCols*mRows; i++) mCharRAM[i] = 0;
            mEscActive = false;
            break;
          }
          case 'K': // clear line from cursor onwards (does not move the cursor)
          {
            int startindex = mYCur * mCols;
            for(int i=startindex + mXCur; i < startindex + mCols; i++) mCharRAM[i] = 0;
            mEscActive = false;
            break;
          }
          case 'S': // scroll up
          {
            ScrollUp();
            mEscActive = false;
            break;
          }
          case 'T': // scroll down
          {
            ScrollDown();
            mEscActive = false;
            break;
          }
          case '[': case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': case '?':
            break; // sammele erstmal diese Ziffern
          default:
            mEscActive = false; // eine undefinierte ESC sequence beendet die ESC sequence
            break;
        }
      } else mEscActive = false; // eine undefinierte ESC sequence beendet die ESC sequence
    }
    else // ES IST KEINE ESC SEQUENCE AKTIV => normale Verarbeitung des Zeichens
    {
      switch (inbyte)
      {
        case '\n': // Sonderzeichen 'newline' abfangen
          mXCur = 0;
          mYCur++;
          if (mYCur > mRows-1) { ScrollUp(); mYCur = mRows - 1; }
          break;
        case '\r': // Sonderzeichen 'carriage return' abfangen
          mXCur = 0;
          break;
        case 8: // Sonderzeichen 'BACKSPACE' abfangen
          if (mXCur > 0) mXCur--;
          mCharRAM[mXCur + mYCur * mCols] = 0;
          break;
        default:
          if (mXCur >= mCols)
          {
            mXCur = 0;
            mYCur++;
            if (mYCur > mRows-1) { ScrollUp(); mYCur = mRows - 1; }
          }
          mCharRAM[mXCur + mYCur * mCols] = inbyte; // einfach das via OUT gesendete Zeichen im Terminal ausgeben
          mXCur++; if (mXCur > mCols)
          {
            mXCur = 0;
            mYCur++;
            if (mYCur > mRows-1) { ScrollUp(); mYCur = mRows - 1; }
          }
          break;
      }
    }
  }
  void ProcessString(std::string str) { for(int i=0; i<str.length(); i++) ProcessChar(uint8_t(str[i])); }
protected:
  void ScrollUp() // helper routines for scrolling up/down the character RAM of the terminal
  {
    memmove(mCharRAM.data(), mCharRAM.data() + mCols, mCols*(mRows-1)); // shift character RAM up
    memset(mCharRAM.data() + mCols*(mRows-1), 0, mCols);
  }
  void ScrollDown()
  {
    memmove(mCharRAM.data() + mCols, mCharRAM.data(), mCols*(mRows-1)); // shift character RAM down
    memset(mCharRAM.data(), 0, mCols);
  }
  std::vector<uint8_t> mCharRAM; // character RAM
  std::string mEscBuffer; // Zwischenspeicher für ESC-Sequenzen, die das Terminal empfängt
  bool mEscActive {false}; // hängt Terminal gerade in einer ESC-Sequenz?
  bool mShowCursor {true}; // Cursor wird gezeigt (kann über ESC-Sequence an/aus geschaltet werden)
  #ifdef TERMINAL_TO_FILE
    std::ofstream mFile;
  #endif
};

class Computer
{
public:
	Computer(TextScreen& usetextscr, Terminal& useterm) : mText(usetextscr), mTerm(useterm)
	{
		for (int j=0; j<8; j++) for (int i=0; i<sizeof(mFLASH[j]); i++) mFLASH[j][i] = 0xff;
		for (int i=0; i<sizeof(mRAM); i++) mRAM[i] = RandomByte(); // 0x00;

		std::ifstream file;
		file.open("flash.bin", std::ios::binary | std::ios::in);
		if (file.is_open())
		{
			file.read((char *)mFLASH, sizeof(mFLASH));
			file.close();
			gFlashLoaded = true;
		}

		Reset();
		mFrequency = 8000000.0f;
		mState = RUN;
	}
	~Computer()
	{
		if (gFlashLoaded)
		{
			std::ofstream outfile("flash.bin", std::ofstream::out | std::ofstream::binary);
			if (outfile.is_open())
			{
				for(int j=0; j<8; j++) for(int i=0; i<0x1000; i++) outfile << mFLASH[j][i];
				outfile.close();
			}
		}
	}
	void Reset() // handle all component's actions here
	{
		mSC = 0; mPC = 0; mFR = 0; gSerialInput = "";
	}
	void DoClock() // handle all component's actions here
	{
		// simulation of the UARTs S-R latch
		if (mUART_Q == false) { mUART_counter = 144; mFlags |= FLAG_R; } // Q=0 halts the clock, loads the 74HC193 counter, sets ~DR=1
		else // case Q=1: 74HC193 counting enabled, counts down until ~TCD = ~DR = 0
		{
			if ((mFlags & FLAG_R) == FLAG_R) // ~DR = CET = 1: data isn't ready yet! => counting stays enabled
			{
				mUART_counter--; // 74HC193 counting action
				if (mUART_counter == 0) mFlags &= ~FLAG_R; // set ~TCD = ~DR = CET = 0 => data is ready now, counter stops with Q=1.
			}
		}
		if (mUART_inputtimer > 0) mUART_inputtimer--; // limits the flow rate of input data
		if (mUART_inputtimer == 0 && gSerialInput.size() > 0) // arrival of some input data? otherwise stay armed...
		{
			uint8_t b = gSerialInput[0]; gSerialInput = gSerialInput.substr(1); // consume a byte of the input buffer
			if (b == 0x0a) mUART_inputtimer = 80000; else mUART_inputtimer = 160; // reset the timer for the next byte (10ms after 0x0a)
			mSerial = b; mUART_Q = true; // datagram on UART_RX initiates the receiving process
		}

		// control word update
		mCtrl = mCtrlROM[ (mFR << 10) | (mIR << 4) | mSC ]; // 4-bit step counter, 6-bit instruction register, 4-bit flags register
		mBus = 0xff; // implements passive "pull-up" bus

		// ALU
		int a = int(mA) & 0xff;
		int b = int(mB) & 0xff;
		if ((mCtrl & ES) == ES) b = (~b) & 0xff; // negation
		int result = a + b + bool(mCtrl & EC); // adder operation
		if ((result & 0xff) == 0) mFlags &= ~FLAG_Z; else mFlags |= FLAG_Z; // update the flags
		if (result > 0xff) mFlags |= FLAG_C; else mFlags &= ~FLAG_C;
		if ((result & 0x80) == 0) mFlags &= ~FLAG_N; else mFlags |= FLAG_N;
		mE = uint8_t(result);

		// outputs
		if ((mCtrl & AO) == AO) mBus = mA;
		if ((mCtrl & BO) == BO) mBus = mB;
		if ((mCtrl & EO) == EO) mBus = mE;
		if ((mCtrl & RO) == RO)
		{
			if (mMAR & 0x8000) mBus = mRAM[mMAR & 0x7fff];
			else mBus = mFLASH[(mMAR>>12) & 0b111][mMAR & 0x0fff];
		}
		if ((mCtrl & TO) == TO) mBus = mSerial; // moves arriving datagram onto the bus, resets the S-R latch
		if ((mCtrl & TR) == TR) mUART_Q = false; // Q reset happen during the same cycle but prior to TI set (see below)
		if ((mCtrl & COL) == COL) mBus = mPC & 0x00ff;
		if ((mCtrl & COH) == COH) mBus = mPC >> 8;

		// inputs
		if ((mCtrl & RI) == RI) // RI|ME/MIL/MIH will work, as ME/MIL/MIH changes MAR *after* write pulse
		{
			if (mMAR & 0x8000) mRAM[mMAR & 0x7fff] = mBus; // write to RAM
			else // FLASH access, mMAR = 0x0000..0x7f00 is asserted
			{
				uint8_t sec = (mMAR>>12) & 0b111;
				switch (mFLASHState)
				{
					case 0: if (mMAR == 0x5555 && mBus == 0xaa) mFLASHState = 1; else mFLASHState = 0; break;
					case 1: if (mMAR == 0x2aaa && mBus == 0x55) mFLASHState = 2; else mFLASHState = 0; break;
					case 2:
						if (mMAR == 0x5555 && mBus == 0xa0) { mFLASHState = 3; break; }
						if (mMAR == 0x5555 && mBus == 0x80) { mFLASHState = 4; break; }
						mFLASHState = 0; break;
					case 3: mFLASH[sec][mMAR & 0x0fff] &= mBus; mFLASHState = 0; break;		// write operation only writes 1->0, not 0->1
					case 4: if (mMAR == 0x5555 && mBus == 0xaa) mFLASHState = 5; else mFLASHState = 0; break;
					case 5: if (mMAR == 0x2aaa && mBus == 0x55) mFLASHState = 6; else mFLASHState = 0; break;
					case 6: if (mBus == 0x30) for (int i=0; i<0x1000; i++) mFLASH[sec][i] = 0xff;	// sector erase operation
						mFLASHState = 0; break;
					default: mFLASHState = 0; break;
				}
			}
		}
		if ((mCtrl & AI) == AI) mA = mBus;
		if ((mCtrl & BI) == BI) mB = mBus;
		if ((mCtrl & FI) == FI) mFR = mFlags & 15;
		if ((mCtrl & II) == II) mIR = mBus & 63;
		if ((mCtrl & CE) == CE) mPC++; // must happen prior to CIL/CIH
		if ((mCtrl & CIL) == CIL) mPC = (mPC & 0xff00) | mBus;
		if ((mCtrl & CIH) == CIH) mPC = (mPC & 0x00ff) | (mBus<<8);
		if ((mCtrl & ME) == ME) mMAR++; // must happen prior to MIL/MIH
		if ((mCtrl & MIL) == MIL) mMAR = (mMAR & 0xff00) | mBus;
		if ((mCtrl & MIH) == MIH) mMAR = (mMAR & 0x00ff) | (mBus<<8);
		if ((mCtrl & TI) == TI) // send char to terminal
		{
			mUART_Q = true; // Q can be reset and set during the same cycle, since TI (set) happens after TR (reset) (see above)
			mTerm.ProcessChar(mBus);
		}
		if ((mCtrl & IC) == IC)
		{
      if (mIR != 0x3c && mIR != 0x3e && mIR != 0x3f) mMips = 0.999995 * mMips + 0.000005 * float(mSC + 1); // measure CPI
			mSC = 0;
		}
		else { mSC++; mSC &= 15; } // step counter increment
	}
	void Update(float deltat) // deltat sollte idealerweise 16.7ms sein
	{
		switch(mState)
		{
			case RUN:
			{
				mSimTime += deltat;
				if (deltat >= 0.02f) mOverload = 0.9f*mOverload + 0.1f; else mOverload = 0.9f*mOverload + 0.0f;
				if (deltat > 0.1f) mSimTime = 0.05f;					// Notbremse!
				else
				{
					float cycletime = 1.0f / mFrequency;
					while (mSimTime > cycletime)
					{
						DoClock();
						mSimTime -= cycletime;
            if (mBreak != 0 && mPC == mBreak) { mState = HALT; mBreak = 0; break; }		// break point reached
					}
				}
				break;
			}
			case NEXTINST:
				do DoClock(); while ((mCtrl & II) == 0);
				mState = HALT;
				break;
			case SINGLESTEP:
				DoClock();
				mState = HALT;
				break;
			case HALT:
				break;
		}
	}
	void Show()
	{
		char buf[100]; mText.Goto(0, 0);
		std::string ctrl[] = { " RO "," RI "," TR "," TO "," TI ","MIH ","MIL "," ME ",
                           " CE ","COH ","COL ","CIH ","CIL "," IC "," FI "," II ",
                           "    "," AO "," AI "," BO "," BI "," EC "," ES "," EO " };
    mText.Color(0); mText.Print("CTRL");
		for(int i=0; i<24; i++) 
		{
			if(mCtrl & (1<<(23-i))) mText.Color(0); else mText.Color(1);
			mText.Print(ctrl[i]);
		}
    mText.Print("\n");

    sprintf(buf,"BUS  %02x\n", mBus); mText.Color(8); mText.Print(buf);

		sprintf(buf, "INST %02x %s step %02d Z=%d N=%d C=%d R=%d\n", mIR, MNEMONICS[mIR & 0x3f].c_str(), mSC & 0xf, (mFR & FLAG_Z) == 0, bool(mFR & FLAG_N), bool(mFR & FLAG_C), bool(mFR & FLAG_R));
		mText.Color(5); mText.Print(buf);

		sprintf(buf, "PC %04x", mPC); mText.Color(7); mText.Print(buf); // PC
		if (mBreak != 0) { sprintf(buf, " BRK %04x\n", mBreak); mText.Color(6); mText.Print(buf); } // breakpoint set
		else mText.Print("\n"); // no breakpoint

		sprintf(buf, "MAR %04x\n", mMAR); // MAR
		mText.Color(7); mText.Print(buf);

		sprintf(buf, "ALU A=%02x B=%02x E=%02x\n",mA, mB, mE);
		mText.Color(0); mText.Print(buf);

		for(uint16_t z=0; z<16; z++) // RAM or FLASH
		{
			sprintf(buf, "%04x ", mShowIndex + (z<<4));
			if (mShowIndex < 0x8000) mText.Color(2); else mText.Color(1);
			mText.Print(buf);
			mText.Color(8);
			for(uint16_t i=0; i<16; i++)
			{
				uint16_t adr = mShowIndex + (z<<4) + i;
				uint8_t inh;
				if (adr & 0x8000) inh = mRAM[adr & 0x7fff];
				else
				{
					uint8_t sec = (adr>>12) & 0b111;
					inh = mFLASH[sec][adr & 0x0fff];
				}
				sprintf(buf, "%02x ", inh);
				if (adr == mMAR) mText.Color(0); else mText.Color(8);
				mText.Print(buf);
			}
			mText.Print("\n");
		}
	}
  void AddToShowIndex(int delta) { mShowIndex += delta; }
	COMPSTATE mState{ HALT };
	float mFrequency{ 0.0f };
	float mOverload{ 0.0f };
	double mMips{ 0.0f };
	uint16_t mBreak{ 0 }; // break point
	uint8_t mBus{ 0 }; // value stored on the bus
	uint32_t mCtrl{ 0 };
	uint8_t mSerial{ 0xff }; // content of the receiving register
	uint8_t mA{ 0 };
	uint8_t mB{ 0 };
	uint8_t mE{ 0 };
	uint8_t mFlags{ 0b1000 }; // flag lines controlled by ALU
	uint16_t mPC{ 0 };
	uint16_t mMAR{ 0 };
	uint8_t mFR{ 0 };
	uint8_t mSC{ 0 };
	uint8_t mIR{ 0 };
	uint32_t mCtrlROM[0x4000] // microcode depending on flags, opcode and stepcounter
	{
		#include "microcode_def.csv"
		#include "microcode_rom.csv"
	};
	uint8_t mRAM[0x8000];
	uint8_t mFLASH[8][0x1000];
	int mFLASHState = 0;
protected:
  uint8_t RandomByte() // helper function to generate pseudo-random RAM content
  {
    static uint8_t RandomByteX {0}, RandomByteA {0}, RandomByteB {0}, RandomByteC {0};
    RandomByteX++;                                                      
    RandomByteA = RandomByteA ^ RandomByteC ^ RandomByteX;
    RandomByteB = RandomByteB + RandomByteA;
    RandomByteC = (RandomByteC + (RandomByteB >> 1)) ^ RandomByteA;
    return RandomByteC;
  }
	float mSimTime{ 0.0f };
	TextScreen& mText; // Components use this screen to display their status
	Terminal& mTerm; // Interaction between computer and terminal is managed here
	uint16_t mShowIndex{ 0 };
	bool mUART_Q{ false }; // S-R latch of the UART interface
	int mUART_counter{ 0 }; // 74HC193 downwards counter of the UART interface
	int mUART_inputtimer{ 0 }; // times the data flow from the terminal to the UART input register
};

class Application // sandbox environment hosting memory, display and the Min interpreter
{
public:
  Application(const Application& other) = delete;
  Application& operator=(const Application& other) = delete;
  Application(GLFWwindow* win, int argc, char *argv[]) : mWin(win)
  {
    glfwSetKeyCallback(win, Application::CallBack_KeyStateChanged); // static member functions as callbacks
    glfwSetCharCallback(win, Application::CallBack_KeyCodePointPressed);
    mIsRunning = true;
    mTerm.SetTint(0.35f, 1.0f, 0.65f, 1.0f); // greenish terminal display
  }
  ~Application() = default;
  void ProcessInput()
  {
		switch(mMenuState)
		{
			case MENU_OFF:
      {
				if (TakeKeyState(GLFW_KEY_F1)) { mMenuState = MENU_SPLASH; gKeyTarget = TARGET_MENU; }
        if (TakeKeyState(GLFW_KEY_F2)) { mMenuState = MENU_STATUS; gKeyTarget = TARGET_MENU; }
				if (TakeKeyState(GLFW_KEY_F12)) { mMenuState = MENU_SPLASH; gKeyTarget = TARGET_MENU; }
				break;
      }
			case MENU_SPLASH:
			{
				if (TakeKeyState(GLFW_KEY_F1)) { mMenuState = MENU_OFF; gKeyTarget = TARGET_SERIAL; }
        if (TakeKeyState(GLFW_KEY_F2)) mMenuState = MENU_STATUS;
				if (TakeKeyState(GLFW_KEY_F12)) mIsRunning = false;
        if (TakeKeyState(GLFW_KEY_F8))				// ROM memory dump of 0x0000-0x0fff in Intel HEX format
        {
          mTerm.ProcessString("\n!Generating!\nctrl_hsb.bin\nctrl_msb.bin\nctrl_lsb.bin\n");
          uint32_t* ctrl = mCPU.mCtrlROM;
          std::ofstream outfile;
          outfile.open("ctrl_lsb.bin", std::ofstream::out | std::ofstream::binary);
          if (outfile.is_open())
          {
            for(int i=0; i<0x4000; i++)
            {
              uint32_t word = ctrl[i] ^ CTRL_INVERT_MASK;
              outfile << uint8_t((word>>0) & 0xff);
            }
            outfile.close();
          }
          outfile.open("ctrl_msb.bin", std::ofstream::out | std::ofstream::binary);
          if (outfile.is_open())
          {
            for(int i=0; i<0x4000; i++)
            {
              uint32_t word = ctrl[i] ^ CTRL_INVERT_MASK;
              outfile << uint8_t((word>>8) & 0xff);
            }
            outfile.close();
          }
          outfile.open("ctrl_hsb.bin", std::ofstream::out | std::ofstream::binary);
          if (outfile.is_open())
          {
            for(int i=0; i<0x4000; i++)
            {
              uint32_t word = ctrl[i] ^ CTRL_INVERT_MASK;
              outfile << uint8_t((word>>16) & 0xff);
            }
            outfile.close();
          }
        }
				
        bool needmoremenu = false;
				while (needmoremenu == false && gMenuInput.size() > 0)     // hier passiert nur noch etwas, wenn serielle Daten vorliegen
				{
					switch(gMenuInput[0])                    		          // werte die verschiedenen Kommandos aus
					{
						case 'l':		// load and assemble a source file
						{
							if (gMenuInput[gMenuInput.size()-1] == 8 && gMenuInput.size() > 0) gMenuInput.erase(gMenuInput.size()-2, 2);
							size_t endofmsg = gMenuInput.find('\n');     // Suche nach dem End-Kontrollzeichen
							if (endofmsg != std::string::npos)           // gibt es auch ein End-Kontrollzeichen? END OF TEXT
							{
								std::string filename = gMenuInput.substr(1, endofmsg-1);
								gMenuInput = gMenuInput.substr(endofmsg);
								filename.erase(std::remove_if(filename.begin(), filename.end(), isspace), filename.end());	// shuffle ALL spaces to end and delete them

								std::ifstream file(filename);
								if (file.is_open())
								{
									std::stringstream hexout, errors;
									std::string source;
									std::getline(file, source, '\0');
									file.close();
									Assembler(source, MNEMONICS, hexout, errors, false, "");
									if (errors.str().size() == 0) gMenuInput += hexout.str(); else mTerm.ProcessString(errors.str());
								} else mTerm.ProcessString("ERROR: File not found.\n");

							} else needmoremenu = true;
							break;
						}
						case ':':   // upload a line of Intel HEX data
						{
							size_t endofmsg = gMenuInput.find('\n');        // Suche nach dem End-Kontrollzeichen
							if (endofmsg != std::string::npos)           // gibt es auch ein End-Kontrollzeichen? END OF TEXT
							{
								std::string line = gMenuInput.substr(1, endofmsg-1);   // Anfang und Ende sind vorhanden => extrahiere das command...
								gMenuInput = gMenuInput.substr(endofmsg);  			       // ... aber behalte den Rest des Puffers NACH diesen Daten
								mTerm.ProcessString(":" + line + "\n");								 // output
								int num, pc, typ;
								std::stringstream snum; snum << std::hex << line.substr(0, 2); line = line.substr(2); snum >> num;
								std::stringstream spc; spc << std::hex << line.substr(0, 4); line = line.substr(4); spc >> pc;
								std::stringstream styp; styp << std::hex << line.substr(0, 2); line = line.substr(2); styp >> typ;
								if (typ == 0) while (num-- > 0)												// data type = data?
								{
									int dat;
									std::stringstream sdat; sdat << std::hex << line.substr(0, 2); line = line.substr(2); sdat >> dat;
									if (pc<0x8000) ((uint8_t*)(mCPU.mFLASH))[pc++] = dat; else mCPU.mRAM[(pc++) & 0x7fff] = dat;	// write datum
								} // don't evaluate the checksum since it's only an emulator ;-)
							} else needmoremenu = true;
							break;
						}
						case '+':
							mCPU.mFrequency = std::min(mCPU.mFrequency + 100000.0f, 8000000.0f);
							mCPU.mState = RUN; gMenuInput = gMenuInput.substr(1); break;
						case '-':
							mCPU.mFrequency = std::max(mCPU.mFrequency - 100000.0f, 100000.0f);
							mCPU.mState = RUN; gMenuInput = gMenuInput.substr(1); break;
						default:
							gMenuInput = gMenuInput.substr(1); break;			// unrecognized menu char is discarded
					}
				}
				break;
			}
			case MENU_STATUS:
			{
        if (TakeKeyState(GLFW_KEY_F3)) { if (mKeyStates[GLFW_KEY_LEFT_SHIFT]) mCPU.AddToShowIndex(-0x1000); else mCPU.AddToShowIndex(-0x0100); }
        if (TakeKeyState(GLFW_KEY_F4)) { if (mKeyStates[GLFW_KEY_LEFT_SHIFT]) mCPU.AddToShowIndex(0x1000); else mCPU.AddToShowIndex(0x0100); }
				if (TakeKeyState(GLFW_KEY_F1)) mMenuState = MENU_SPLASH;
				if (TakeKeyState(GLFW_KEY_F2)) { mMenuState = MENU_OFF; gKeyTarget = TARGET_SERIAL; }
				if (TakeKeyState(GLFW_KEY_F12)) mMenuState = MENU_SPLASH;
				bool needmoremenu = false;
				while (needmoremenu == false && gMenuInput.size() > 0)     // hier passiert nur noch etwas, wenn serielle Daten vorliegen
				{
					switch(gMenuInput[0])                    		          // werte die verschiedenen Kommandos aus
					{
						case 's': mCPU.mState = SINGLESTEP; gMenuInput = gMenuInput.substr(1); break;
						case 'x': mCPU.mState = NEXTINST; gMenuInput = gMenuInput.substr(1); break;
						case 'b':		// set a break point
							if (gMenuInput[gMenuInput.size()-1] == 8 && gMenuInput.size() > 0) gMenuInput.erase(gMenuInput.size()-2, 2);
							if (gMenuInput.size() >= 5)                     // gibt es auch ein End-Kontrollzeichen? END OF TEXT
							{
								std::stringstream sadr; sadr << std::hex << gMenuInput.substr(1, 4);
								gMenuInput = gMenuInput.substr(5);
								sadr >> mCPU.mBreak;
							} else needmoremenu = true;
							break;
						case 'p':		// change program counter
							if (gMenuInput[gMenuInput.size()-1] == 8 && gMenuInput.size() > 0) gMenuInput.erase(gMenuInput.size()-2, 2);
							if (gMenuInput.size() >= 5)                              // gibt es auch ein End-Kontrollzeichen? END OF TEXT
							{
								std::stringstream sadr; sadr << std::hex << gMenuInput.substr(1, 4);
								gMenuInput = gMenuInput.substr(5);
								sadr >> mCPU.mPC;
								mCPU.mState = HALT;
							} else needmoremenu = true;
							break;
						case '+':
							mCPU.mFrequency = std::min(mCPU.mFrequency + 100000.0f, 80000000.0f);
							mCPU.mState = RUN; gMenuInput = gMenuInput.substr(1); break;
						case '-':
							mCPU.mFrequency = std::max(mCPU.mFrequency - 100000.0f, 100000.0f);
							mCPU.mState = RUN; gMenuInput = gMenuInput.substr(1); break;
						default:
							gMenuInput = gMenuInput.substr(1); break;			// unrecognized menu char is discarded
					}
				}
				break;
			}
			default:;
		}
		if (TakeKeyState(GLFW_KEY_F11)) { mCPU.Reset(); }
		if (TakeKeyState(GLFW_KEY_F10))
		{
			const char* clip = glfwGetClipboardString(mWin);
			if (clip) gSerialInput += clip; // appends the entire string at once
		}
  }
  void Update(float akttime, float deltat)
  {
		UpdateFpsInfo(akttime, mFpsInfo);
		mCPU.Update(deltat);				// start CPU after deltat is stable
  }
  void Render()
  {
		mText.Clear();
		switch(mMenuState)
		{
			case MENU_SPLASH:
			{
        mTerm.SetTint(0.35f, 1.0f, 0.65f, 0.25f);
        const int tx = 22, ty = 10;
				mText.Print(tx, ty+0,5,  "Minimal UART CPU 3.3 Emulator by C. Herting (slu4) 2026");
				if (!gFlashLoaded) mText.Print(tx, ty+1,2, "ERROR: Can't find SSD image file 'flash.bin'.");
				mText.Print(tx, ty+3,0,  "[ F1] - Toggles this menu.");
        mText.Print(tx, ty+4,0,  "[ F2] - Toggles CPU monitor.");
        mText.Print(tx, ty+5,1,  "[ F8] - Saves CTRL FLASH dump.");
				mText.Print(tx, ty+6,7,  "[F10] - Feeds clipboard data as UART input.");
				mText.Print(tx, ty+7,7,  "[F11] - RESET");
				mText.Print(tx, ty+8,7,  "[F12] - Exits and saves SSD image to 'flash.bin'.");
        if (gMenuInput.size() > 0) mText.Print(0, 29, 0, gMenuInput);
        mText.Print(tx, ty+12,0, "CPU clock rate = " + std::to_string(int(mCPU.mFrequency)) + "Hz (use [+] and [-])");
				mText.Print(tx, ty+13,0, "Frame rate = " + std::to_string(int(mFpsInfo)) + "fps.");
        break;
			}
			case MENU_STATUS:
        mTerm.SetTint(0.35f, 1.0f, 0.65f, 0.25f);
        mCPU.Show();
				mText.Print(0, 23, 1, "[F3/4]       Dec/inc RAM monitor by 0x0100");
				mText.Print(0, 24, 1, "[SHIFT F3/4] Dec/inc RAM monitor by 0x1000");
				mText.Print(0, 26, 1, std::to_string(int(mCPU.mFrequency)) + "Hz");
				mText.Print(15, 26, 1, std::to_string(int(mFpsInfo)) + "fps");
        mText.Print(30, 26, 1, std::to_string(mCPU.mMips) + "CPI");
				if (gMenuInput.size() > 0) mText.Print(0, 29, 0, gMenuInput);
        break;
			default: mTerm.SetTint(0.35f, 1.0f, 0.65f, 1.0f); break;
		}

    mTerm.Render();
    if (mCPU.mOverload > 0.5f) mText.Print(45, 26, 2, "Clockrate too high!");
		mText.Render();
  }
  bool isRunning() { return mIsRunning; }
private:
  void UpdateFpsInfo(float akttime, float& fpsinfo)
	{
		static float lastfpstime = akttime;					// einmalige Initialisierung
		static int framecounter = -1;								// einmalige Initialisierung
		framecounter++;
		float elapsedfpstime = akttime - lastfpstime;
		if (elapsedfpstime > 1.0f)
		{
			lastfpstime = akttime;
			fpsinfo = float(framecounter) / elapsedfpstime;
			framecounter = 0;
		}
	}
  bool TakeKeyState(int key) { return std::exchange(mKeyStates[key], false); }
  static void CallBack_KeyStateChanged(GLFWwindow* win, int key, int scancode, int action, int mods)
  {
    if (key >= 0 && key < 1024)
    {
      if (action == GLFW_RELEASE) mKeyStates[key] = false;
      else if (action == GLFW_PRESS) mKeyStates[key] = true;
      else if (action == GLFW_REPEAT) mKeyStates[key] = true;
    }
    if (key >= GLFW_KEY_F1 && key <= GLFW_KEY_F12) return;	// don't translate function keys into serial or PS2 input

    switch(gKeyTarget)
    {
      case TARGET_SERIAL:
      {
        if (action == GLFW_PRESS || action == GLFW_REPEAT)
        {
          if (mKeyStates[GLFW_KEY_LEFT_CONTROL] || mKeyStates[GLFW_KEY_RIGHT_CONTROL])
          {
            if (key == GLFW_KEY_Q) gSerialInput += char(0x11);
            if (key == GLFW_KEY_Y) gSerialInput += char(0x1a);
            if (key == GLFW_KEY_X) gSerialInput += char(0x18);
            if (key == GLFW_KEY_C) gSerialInput += char(0x03);
            if (key == GLFW_KEY_V) gSerialInput += char(0x16);
            if (key == GLFW_KEY_A) gSerialInput += char(0x01);
            if (key == GLFW_KEY_S) gSerialInput += char(0x13);
            if (key == GLFW_KEY_L) gSerialInput += char(0x0c);
            if (key == GLFW_KEY_N) gSerialInput += char(0x0e);
            if (key == GLFW_KEY_R) gSerialInput += char(0x12);
            if (key == GLFW_KEY_T) gSerialInput += char(0x14);
          }
          if (key == GLFW_KEY_ESCAPE) gSerialInput += '\e';
          if (key == GLFW_KEY_ENTER) gSerialInput += '\n';
          if (key == GLFW_KEY_KP_ENTER) gSerialInput += '\n';
          if (key == GLFW_KEY_UP) gSerialInput += "\e[A";
          if (key == GLFW_KEY_TAB) gSerialInput += "  ";
          if (key == GLFW_KEY_DOWN) gSerialInput += "\e[B";
          if (key == GLFW_KEY_RIGHT) gSerialInput += "\e[C";
          if (key == GLFW_KEY_LEFT) gSerialInput += "\e[D";
          if (key == GLFW_KEY_KP_8) gSerialInput += "\e[A";
          if (key == GLFW_KEY_KP_2) gSerialInput += "\e[B";
          if (key == GLFW_KEY_KP_6) gSerialInput += "\e[C";
          if (key == GLFW_KEY_KP_4) gSerialInput += "\e[D";
          if (key == GLFW_KEY_END) gSerialInput += "\e[4~";
          if (key == GLFW_KEY_HOME) gSerialInput += "\e[1~";
          if (key == GLFW_KEY_KP_7) gSerialInput += "\e[1~";
          if (key == GLFW_KEY_KP_1) gSerialInput += "\e[4~";
          if (key == GLFW_KEY_PAGE_UP || key == GLFW_KEY_KP_9) gSerialInput += "\e[5~";
          if (key == GLFW_KEY_PAGE_DOWN || key == GLFW_KEY_KP_3) gSerialInput += "\e[6~";
          if (key == GLFW_KEY_DELETE) gSerialInput += 127;
          if (key == GLFW_KEY_KP_DECIMAL) gSerialInput += 127;
          if (key == GLFW_KEY_BACKSPACE) gSerialInput += 8;
        }
        break;
      }
      case TARGET_MENU:
      {
        if (action == GLFW_PRESS)
        {
          if (key == GLFW_KEY_BACKSPACE) gMenuInput += 8;
          if (key == GLFW_KEY_ENTER) gMenuInput += '\n';
        }
        break;
      }
    }
  }
  static void CallBack_KeyCodePointPressed(GLFWwindow* win, GLuint codepoint)
  {
    switch(gKeyTarget)
    {
      case TARGET_SERIAL: if(codepoint) gSerialInput += codepoint; break;
      case TARGET_MENU: if(codepoint) gMenuInput += codepoint; break;
    }
  }

  GLFWwindow* mWin;
  bool mIsRunning {false};
  static bool mKeyStates[1024]; // state of all keys
  Terminal mTerm {(const uint32_t*)&_binary_charset_bin_start, 50, 30};
  TextScreen mText {(const uint32_t*)&_binary_charset_bin_start, 100, 30};
	float mFpsInfo {60.0f};																// gemessene Anzahl 'frames per second'
	Computer mCPU {mText, mTerm};
	uint16_t mShowIndex {0};
	enum MENUSTATE {MENU_SPLASH, MENU_OFF, MENU_STATUS} mMenuState {MENU_SPLASH};
};

bool Application::mKeyStates[1024] {false}; // static Application member: state of all keys (only accessed from this thread)

int main(int argc, char *argv[]) // OpenGL setup and launch of Application class
{
  if (glfwInit() == GLFW_FALSE) { std::cerr << "glfwInit() failed\n"; return 0; }
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_SAMPLES, 0);

  const GLFWvidmode* mode = glfwGetVideoMode(glfwGetPrimaryMonitor());
  if (mode == nullptr) { std::cerr << "glfwGetVideoMode() failed\n"; return 0; }
  glfwWindowHint(GLFW_RED_BITS, mode->redBits);
  glfwWindowHint(GLFW_GREEN_BITS, mode->greenBits);
  glfwWindowHint(GLFW_BLUE_BITS, mode->blueBits);
  glfwWindowHint(GLFW_REFRESH_RATE, mode->refreshRate);

  GLFWwindow* win = glfwCreateWindow(mode->width, mode->height, "OpenGL", glfwGetPrimaryMonitor(), nullptr); // FULLSCREEN: glfwGetPrimaryMonitor(), WINDOWED: nullptr
  if (win == nullptr) { std::cerr << "glfwCreateWindow() failed\n"; glfwTerminate(); return 0; } // glfwTerminate() is new
  glfwMakeContextCurrent(win);
  glfwSwapInterval(1);
  glfwSetInputMode(win, GLFW_CURSOR, GLFW_CURSOR_DISABLED);

  if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress)) // Init GLAD OpenGL-Extension-Loader BEFORE any OpenGL calls are made
  {
    std::cerr << "gladLoadGLLoader() failed\n";
    glfwDestroyWindow(win); glfwTerminate(); return 0;
  } 

  glDisable(GL_MULTISAMPLE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // normal blending
  glBlendEquation(GL_FUNC_ADD);
  glDisable(GL_DEPTH_TEST); // disable depth-testing
  glDepthMask(GL_FALSE); // disable writing to depth buffer

  try // exceptions are thrown by the application to report errors during display setup, shader loading etc.
  {
    float lasttime = float(glfwGetTime());
    Application app(win, argc, argv); // create main application
    while (app.isRunning()) // MAIN LOOP OF APPLICATION
    {
      float akttime = float(glfwGetTime());
      float deltat = akttime - lasttime;
      lasttime = akttime;
      glfwPollEvents(); // update event loop
      app.ProcessInput(); // injects keypresses      
      app.Update(akttime, deltat); // injects time
      glDepthMask(GL_TRUE); // enable write to depth buffer, das muss vor glClear passieren!
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); // colors are cleared, stencil and z buffer could also be cleared
      app.Render();
      glfwSwapBuffers(win);
      std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
  }
  catch (const std::exception& e) { std::cerr << e.what() << std::endl; }
  catch (...) { std::cerr << "Unknown app exception\n"; }

  glfwDestroyWindow(win); // close OpenGL window first
  glfwTerminate(); // destroy context, window and all other OpenGL stuff
  return 0;
}
