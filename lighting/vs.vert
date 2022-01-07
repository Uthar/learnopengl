#version 130

in vec3 vertexPos;
in vec3 vertexNormal;
in vec2 vertexTexCoords;

out vec3 fragNormal;
out vec3 fragPos;
out vec2 fragTexCoords;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
uniform mat3 normal;

void main() {
  gl_Position = projection * view * model * vec4(vertexPos, 1.0);
  fragNormal = normal * vertexNormal;
  fragPos = vec3(model * vec4(vertexPos, 1.0));
  fragTexCoords = vertexTexCoords;
}
