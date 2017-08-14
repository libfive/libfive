#version 330

layout(location=0) in vec3 vertex_position;
layout(location=1) in vec3 vertex_color;

uniform mat4 M;

out vec3 frag_color;
out vec3 frag_pos;
out vec3 ec_pos;

void main()
{
    frag_pos = vertex_position;
    gl_Position = M * vec4(vertex_position, 1.0f);
    ec_pos = gl_Position.xyz;
    frag_color = vertex_color;
}
