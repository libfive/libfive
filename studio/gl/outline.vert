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
    //Simple pass-through
    //transformation matrix is handled in the geometry shader
    gl_Position = vec4(vertex_position, 1);
}
