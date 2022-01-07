#version 130

in vec3 vertexPos;
in vec3 vertexNormal;

out vec3 fragNormal;
out vec3 fragPos;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
uniform mat3 normal;

void main() {
  gl_Position = projection * view * model * vec4(vertexPos, 1.0);
  fragNormal = normal * vertexNormal;
  fragPos = vec3(model * vec4(vertexPos, 1.0));
}
