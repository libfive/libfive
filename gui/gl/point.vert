#version 330

layout(location=0) in vec3 vertex_position;

uniform mat4 M;
uniform vec4 color;

out vec3 frag_pos;
out vec4 frag_color;

void main()
{
    gl_Position = M * vec4(vertex_position, 1.0f);
    frag_color = color;
}

