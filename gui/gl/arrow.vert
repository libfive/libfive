#version 330

layout(location=0) in vec3 vertex_position;
layout(location=1) in vec3 vertex_norm;

uniform mat4 M;

out vec3 frag_norm;
out vec3 frag_pos;

void main()
{
    gl_Position = M * vec4(vertex_position, 1.0f);
    gl_Position.w = max(0, gl_Position.w);
    frag_pos = gl_Position.xyz / gl_Position.w;

    frag_norm = (M * vec4(vertex_position + vertex_norm, 1.0f)).xyz - frag_pos;
    frag_norm.z *= -8;
    frag_norm = normalize(frag_norm);
}

