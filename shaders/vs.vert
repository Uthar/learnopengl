#version 130

in vec3 pos;
in vec2 texcoords;

out vec4 fragPos;
out vec2 texPos;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
  fragPos = projection * view * model * vec4(pos, 1.0);
  texPos = texcoords;
  gl_Position = fragPos;
}
