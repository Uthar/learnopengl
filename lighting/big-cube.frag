#version 130

in vec3 fragNormal;
in vec3 fragPos;
in vec2 fragTexCoords;

out vec4 fragColor;

struct Material {
  sampler2D diffuse;
  sampler2D specular;
  sampler2D emission;
  float shininess;
};

struct DirLight {
  vec3 direction;
  vec3 ambient;
  vec3 diffuse;
  vec3 specular;
};

struct PointLight {
  vec3 position;
  float constant;
  float linear;
  float quadratic;
  vec3 ambient;
  vec3 diffuse;
  vec3 specular;
};

struct SpotLight {
  vec3 position;
  vec3 direction;
  float cutoff;
  float outer;
  vec3 ambient;
  vec3 diffuse;
  vec3 specular;
};

uniform vec3 viewPos;
uniform float time;
uniform Material material;
uniform DirLight dirLight;

#define MAX_POINT_LIGHTS 128
uniform PointLight pointLights[MAX_POINT_LIGHTS];
uniform int numPointLights;

#define MAX_SPOT_LIGHTS 128
uniform SpotLight spotLights[MAX_SPOT_LIGHTS];
uniform int numSpotLights;

vec3 calcDirLight(DirLight light, vec3 viewDirection, vec3 normal) {
  vec3 diffuseTexColor = vec3(texture(material.diffuse, fragTexCoords));
  vec3 specularTexColor = vec3(texture(material.specular, fragTexCoords));

  vec3 lightDir = normalize(light.direction);
  vec3 norm = normalize(normal);
  vec3 viewDir = normalize(viewDirection);

  vec3 ambient = light.ambient * diffuseTexColor;

  float diff = max(dot(-lightDir, norm), 0.0);
  vec3 diffuse = diff * light.diffuse * diffuseTexColor;

  float spec = pow(max(dot(reflect(-lightDir, norm), -viewDir), 0.0), material.shininess);
  vec3 specular = spec * light.specular * specularTexColor;

  return ambient + diffuse + specular;
};

vec3 calcPointLight(PointLight light, vec3 viewDirection, vec3 normal, vec3 position) {
  vec3 diffuseTexColor = vec3(texture(material.diffuse, fragTexCoords));
  vec3 specularTexColor = vec3(texture(material.specular, fragTexCoords));

  vec3 lightDir = normalize(light.position - position);
  vec3 norm = normalize(normal);
  vec3 viewDir = normalize(viewDirection);

  vec3 ambient = light.ambient * diffuseTexColor;

  float diff = max(dot(lightDir, norm), 0.0);
  vec3 diffuse = diff * light.diffuse * diffuseTexColor;

  float spec = pow(max(dot(reflect(lightDir, norm), -viewDir), 0.0), material.shininess);
  vec3 specular = spec * light.specular * specularTexColor;

  float distance    = length(light.position - position);
  float attenuation = 1.0 / (light.constant + light.linear * distance +
                             light.quadratic * (distance * distance));

  // return vec3(1.0);
  return attenuation * (ambient + diffuse + specular);
};

vec3 calcSpotLight(SpotLight light, vec3 viewDirection, vec3 normal, vec3 position) {
  vec3 diffuseTexColor = vec3(texture(material.diffuse, fragTexCoords));
  vec3 lightDir = normalize(light.position - position);

  float theta = dot(lightDir, normalize(light.direction));
  float epsilon = light.cutoff - light.outer;
  float intensity = clamp((theta  - light.outer) / epsilon, 0.0, 1.0);

  vec3 ambient = light.ambient * diffuseTexColor;

  if (theta < light.outer) {
    return ambient;
  };

  vec3 specularTexColor = vec3(texture(material.specular, fragTexCoords));

  vec3 norm = normalize(normal);
  vec3 viewDir = normalize(viewDirection);

  float diff = max(dot(lightDir, norm), 0.0);
  vec3 diffuse = diff * light.diffuse * diffuseTexColor;

  float spec = pow(max(dot(reflect(lightDir, norm), -viewDir), 0.0), material.shininess);
  vec3 specular = spec * light.specular * specularTexColor;

  diffuse *= intensity;
  specular *= intensity;
  return (ambient + diffuse + specular);
};

void main() {

  vec3 color = vec3(0.0);

  color += calcDirLight(dirLight, (viewPos - fragPos), fragNormal);

  for (int i = 0; i < numPointLights; i++) {
    color += calcPointLight(pointLights[i], (viewPos - fragPos), fragNormal, fragPos);
  }

  for (int i = 0; i < numSpotLights; i++) {
    color += calcSpotLight(spotLights[i], (viewPos - fragPos), fragNormal, fragPos);
  }

  fragColor = vec4(color, 1.0);
}
