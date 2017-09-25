#version 330

layout(location=0) in vec3 vertex_position;
layout(location=1) in float vertex_i;

uniform mat4 M;
uniform vec3 corner_min;
uniform vec3 corner_max;

out vec3 frag_bary;

void main()
{
    vec3 scale = corner_max - corner_min;
    gl_Position = M * vec4(vertex_position * scale + corner_min, 1.0f);

    frag_bary = vec3(0.0f, 0.0f, 0.0f);
    frag_bary[int(vertex_i)] = 1.0f;
}

