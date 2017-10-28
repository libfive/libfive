#version 330

layout(location=0) in vec3 vertex_position;
layout(location=1) in vec3 vertex_norm;
layout(location=2) in float vertex_sharp;

uniform mat4 M;

out vec3 frag_pos;
out vec3 frag_norm;
out float frag_sharp;
out vec3 ec_pos;

void main()
{
    frag_pos = vertex_position;
    gl_Position = M * vec4(vertex_position, 1.0f);
    gl_Position.w = max(0, gl_Position.w);

    ec_pos = gl_Position.xyz / gl_Position.w;

    frag_sharp = vertex_sharp;

    // Calculate normal by finding the position of an offset point, then
    // subtracting out the original position.
    if (length(vertex_norm) > 0.0f)
    {
        vec4 offset = M * vec4(vertex_position + vertex_norm, 1.0f);
        frag_norm = offset.xyz / offset.w - ec_pos;
        frag_norm.z /= 8.0f;
        frag_norm = normalize(frag_norm);
    }
    else
    {
        frag_norm = vec3(0.0f, 0.0f, 0.0f);
    }
}

