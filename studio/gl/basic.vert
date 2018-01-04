#version 330

layout(location=0) in vec3 vertex_position;
layout(location=1) in vec3 vertex_color;

uniform mat4 M;

out vec4 frag_color;
out vec3 frag_pos;
out vec3 ec_pos;

void main()
{
    frag_pos = vertex_position;
    gl_Position = M * vec4(vertex_position, 1.0f);
    gl_Position.w = max(0, gl_Position.w);

    ec_pos = gl_Position.xyz / gl_Position.w;
    frag_color = vec4(vertex_color, 1.0f);
}
