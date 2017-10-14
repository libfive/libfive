#version 330

layout(location=0) in vec3 vertex_position;

uniform mat4 M;
out vec3 frag_pos;
out vec3 frag_color;

void main()
{
    gl_Position = vec4(vertex_position, 1.0f); //M * vec4(vertex_position, 1.0f);
    frag_color = vec3(1.0f, 1.0f, 1.0f);
}

