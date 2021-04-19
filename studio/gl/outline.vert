#version 150
#extension GL_ARB_explicit_attrib_location : require

layout(location=0) in vec3 vertex_position;
layout(location=1) in vec3 vertex_color;

uniform mat4 M;

out vec4 frag_color;
out vec3 base_pos;
out vec3 frag_pos;
out vec3 frag_norm;

void main()
{
    // base_pos = vertex_position;

    // gl_Position = M * vec4(vertex_position, 1.0f);
    // gl_Position.w = max(0, gl_Position.w);

    // frag_pos = gl_Position.xyz / gl_Position.w;
    // frag_color = vec4(vertex_color, 1.0f);

    // vec4 norm_pos = M * vec4(vertex_position + vertex_norm, 1.0f);
    // frag_norm = (norm_pos.xyz / norm_pos.w) - frag_pos;
    // frag_norm.z *= -8;
    // frag_norm = normalize(frag_norm);


    gl_Position = vec4(vertex_position, 1);
}
