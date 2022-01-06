#version 130

in vec3 norm;
in vec3 pos;

out vec4 fragColor;

uniform vec3 lightPos;

void main() {

  float ambientStrength = 0.2;
  vec3 lightColor = vec3(1.0);
  vec3 objectColor = vec3(0.8, 0.6, 0.2);
  vec3 ambient = ambientStrength * lightColor;

  vec3 nnormal = normalize(norm);

  vec3 lightDir = normalize(lightPos - pos);
  float diff = max(dot(nnormal, lightDir), 0.0);
  vec3 diffuse = diff * lightColor;

  vec3 color = (ambient + diffuse) * objectColor;
  // vec3 color = ambient * objectColor;
  // vec3 color = objectColor;

  fragColor = vec4(color, 1.0);
}
