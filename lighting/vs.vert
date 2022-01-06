#version 130

in vec3 vertexData;
in vec3 vertexNormal;

out vec3 norm;
out vec3 pos;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
uniform mat3 normal;

void main() {
  gl_Position = projection * view * model * vec4(vertexData, 1.0);
  norm = normal * vertexNormal;
  pos = vec3(model * vec4(vertexData, 1.0));
}
