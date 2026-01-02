# use 'make' or 'make linux' to build on Linux for Linux
# use 'make windows' to build either on Windows/MSYS2 or Linux for Windows

VPATH					:= include/glad
CFLAGS				:= -O2 -std=c++20 -Iinclude
BUILD					?= linux
GLFW_LINUX_A	:= lib/libglfw3linux.a
GLFW_WIN_A		:= lib/libglfw3win.a

ifeq ($(BUILD),windows)
	CXX					:= x86_64-w64-mingw32-g++
	LDFLAGS			:= -s -static -Llib -static-libgcc -static-libstdc++
	LIBS				:= $(GLFW_WIN_A) -lopengl32 -lgdi32 # optional: luser32 -lkernel32 -lshell32 -lwinmm
	BFDNAME			:= pei-x86-64
	ifeq ($(OS),Windows_NT)
		OBJCOPY		:= objcopy
	else
		OBJCOPY		:= x86_64-w64-mingw32-objcopy
	endif
else
	CXX					:= g++
	LDFLAGS			:= -s -pthread -Llib
	LIBS				:= $(GLFW_LINUX_A) -lrt -lm -ldl -lX11 -lXrandr -lXinerama -lXxf86vm -lXcursor -lGL
	BFDNAME			:= elf64-x86-64
	OBJCOPY			:= objcopy
endif

.PHONY: linux windows clean
linux:
	$(CXX) asm.cpp -O2 -oasm -s
	@$(MAKE) BUILD=linux MinimalUART.exe

windows:
	$(CXX) asm.cpp -O2 -oasm.exe -s -static
	@$(MAKE) BUILD=windows MinimalUART.exe

clean:
	rm -f *.o *.exe

MinimalUART.exe: main.o glad.o resources.o
	$(CXX) $(LDFLAGS) -L$(3RDPARTY)/lib -o$@ $^ $(LIBS)

main.o: main.cpp
	$(CXX) $(CFLAGS) -c $<

glad.o: glad.c glad.h
	$(CXX) $(CFLAGS) -c $<

resources.o: shader.glsl charset.bin
	$(OBJCOPY) -Ibinary -O$(BFDNAME) -Bi386:x86-64 shader.glsl shader.o
	$(OBJCOPY) -Ibinary -O$(BFDNAME) -Bi386:x86-64 charset.bin charset.o
	$(CXX) -r -o$@ shader.o charset.o
ifneq ($(BUILD),windows)
	$(OBJCOPY) --add-section .note.GNU-stack=/dev/null --set-section-flags .note.GNU-stack=contents,readonly $@
endif
