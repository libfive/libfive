#version 150
#extension GL_ARB_explicit_attrib_location : require

layout(location=0) in vec2 vertex_position;

uniform vec3 a;
uniform vec3 b;
uniform float thickness;
uniform float aspect;

// For compatability with base shader
out vec3 frag_norm;
out vec3 frag_pos;
out vec4 frag_color;

void main()
{
    vec2 offset;
    vec3 target;

    if (vertex_position.y < 0.5)
    {
        offset = vertex_position;
        target = a;
    }
    else
    {
        offset = vertex_position - vec2(0, 1);
        target = b;
    }

    // Adjust for line thickness
    offset *= thickness;

    float angle = atan((b - a).x, (b - a).y);

    mat2 rot = mat2(cos(angle), -sin(angle),
                    sin(angle), cos(angle));
    offset = rot * offset;

    if (aspect > 1.0f)
    {
        offset.y *= aspect;
    }
    else
    {
        offset.x /= aspect;
    }

    gl_Position = vec4(target.xy + offset, target.z, 1.0f);

    frag_norm = vec3(0.0f);
    frag_pos = vec3(0.0f);
    frag_color = vec4(0.0f);
}

