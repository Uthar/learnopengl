#version 130

in vec3 fragNormal;
in vec3 fragPos;
in vec2 fragTexCoords;

out vec4 fragColor;

struct Material {
  sampler2D diffuse;
  sampler2D specular;
  float shininess;
};

struct Light {
  vec3 position;
  vec3 ambient;
  vec3 diffuse;
  vec3 specular;
};

uniform vec3 viewPos;
uniform Material material;
uniform Light light;

void main() {

  vec3 diffuseTexColor = vec3(texture(material.diffuse, fragTexCoords));
  vec3 ambient = light.ambient * diffuseTexColor;

  vec3 normal = normalize(fragNormal);

  vec3 lightDir = normalize(light.position - fragPos);
  float diff = max(dot(normal, lightDir), 0.0);
  vec3 diffuse = diff * light.diffuse * diffuseTexColor;

  vec3 viewDir = normalize(viewPos - fragPos);
  vec3 reflection = reflect(-lightDir, normal);
  float spec = pow(max(dot(viewDir, reflection), 0.0), material.shininess);
  vec3 specularTexColor = vec3(texture(material.specular, fragTexCoords));
  vec3 specular = spec * light.specular * specularTexColor;

  vec3 color = ambient + diffuse + specular;
  fragColor = vec4(color, 1.0);
}
