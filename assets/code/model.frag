#version 330 core

struct Material {
  float shininess;
  sampler2D diffuse; sampler2D specular;
};

struct PointLight {
  vec3 position;
  vec3 ambient; vec3 diffuse; vec3 specular;
  float constant; float linear; float quadratic;
};

struct DirLight {
  vec3 direction;
  vec3 ambient; vec3 diffuse; vec3 specular;
};

out vec4 FragColor;

in vec3 fragPos; in vec3 normal; in vec2 texCoord;

uniform vec3 viewPos;

#define NR_POINT_LIGHTS 4

uniform DirLight dirLight;
uniform PointLight pointLights[NR_POINT_LIGHTS];
uniform Material material;

vec3 calcDirLight(DirLight light, vec3 normal, vec3 viewDir);
vec3 calcPointLight(PointLight light, vec3 normal, vec3 viewDir, vec3 fragPos);

void main()
{
    // Diffuse
    vec3 norm = normalize(normal);
    vec3 viewDir = normalize(viewPos - fragPos);

    vec3 result = calcDirLight(dirLight, norm, viewDir);

    for (int i = 0; i < NR_POINT_LIGHTS; i++)
      result += calcPointLight(pointLights[i], norm, viewDir, fragPos);

    FragColor = vec4(result, 1.0);
}

vec3 calcDirLight(DirLight light, vec3 normal, vec3 viewDir)
{
  vec3 lightDir = normalize(-light.direction);
  vec3 reflectDir = reflect(-lightDir, normal);

  float diff = max(dot(normal, lightDir), 0.0);
  float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);

  vec3 ambient = light.ambient * vec3(texture(material.diffuse, texCoord));
  vec3 diffuse = light.diffuse * diff * vec3(texture(material.diffuse, texCoord));
  vec3 specular = light.specular * spec * vec3(texture(material.specular, texCoord));

  return (ambient + diffuse + specular);
}

vec3 calcPointLight(PointLight light, vec3 normal, vec3 viewDir, vec3 fragPos)
{
  vec3 lightDir = normalize(light.position - fragPos);
  vec3 reflectDir = reflect(-lightDir, normal);
  
  float diff = max(dot(normal, lightDir), 0.0);
  float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
  
  float distance = length(light.position - fragPos);
  float attenuation = 1.0 / (light.constant +
			     light.linear * distance +
			     light.quadratic * (distance * distance));

  vec3 ambient = light.ambient * vec3(texture(material.diffuse, texCoord));
  vec3 diffuse = light.diffuse * diff * vec3(texture(material.diffuse, texCoord));
  vec3 specular = light.specular * spec * vec3(texture(material.specular, texCoord));

  ambient *= attenuation;
  diffuse *= attenuation;
  specular *= attenuation;
  
  return (ambient + diffuse + specular);
}
