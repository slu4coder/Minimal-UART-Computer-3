SHADER VERTEX
#version 330 core

layout (location = 0) in vec2 position;
layout (location = 1) in vec2 texcoord;

uniform mat4 proj;

out vec2 TexCoord;

void main()
{
  gl_Position = proj * vec4(position.x, position.y, -0.999999f, 1.0f);
  TexCoord = vec2(texcoord.x, texcoord.y);
}
SHADER END

SHADER FRAGMENT
#version 330 core

in vec2 TexCoord;                     // erhält die (interpolierten) Koordinaten des zur (interpolierten Position passenden) Texels

uniform sampler2D drawtexunit;        // has to be set to active texture unit (default: GL_TEXTURE0)
uniform vec4 tint;

out vec4 OutColor;                    // Output des fragment shaders

void main()
{
  OutColor = tint * texture(drawtexunit, TexCoord);
}
SHADER END
