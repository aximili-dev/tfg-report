#version 330 core

out vec4 FragColor;

in vec3 fragPos;
in vec4 vertColor;

uniform vec3 viewPos;

void main()
{
    FragColor = vertColor;
}
