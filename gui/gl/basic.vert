#version 330

layout(location=0) in vec3 vertex_position;
layout(location=1) in vec3 vertex_color;

uniform mat4 M;

out vec3 frag_color;

void main()
{
    gl_Position = M * vec4(vertex_position, 1.0f);
    frag_color = vertex_color;
}
