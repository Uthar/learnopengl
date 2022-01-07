#version 130

in vec3 norm;
in vec3 pos;

out vec4 fragColor;

uniform vec3 lightPos;
uniform vec3 viewPos;

void main() {

  float ambientStrength = 0.2;
  vec3 lightColor = vec3(1.0);
  vec3 objectColor = vec3(0.8, 0.6, 0.2);
  vec3 ambient = ambientStrength * lightColor;

  vec3 nnormal = normalize(norm);

  vec3 lightDir = normalize(lightPos - pos);
  float diff = max(dot(nnormal, lightDir), 0.0);
  vec3 diffuse = diff * lightColor;

  vec3 viewDir = normalize(viewPos - pos);
  vec3 reflection = reflect(-lightDir, nnormal);
  float spec = pow(max(dot(viewDir, reflection), 0.0), 32);
  vec3 specular = spec * 0.5 * lightColor;

  vec3 color = (ambient + diffuse + specular) * objectColor;
  // vec3 color = ambient * objectColor;
  // vec3 color = objectColor;

  fragColor = vec4(color, 1.0);
}
