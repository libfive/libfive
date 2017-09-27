#version 330

layout(location=0) in vec2 vertex_position;

uniform vec3 a;
uniform vec3 b;
uniform float thickness;
uniform float aspect;

void main()
{
    vec2 delta = (b - a).xy;
    float scale = length(delta);

    // Adjust line thickness
    float z = 0.0f;
    vec2 v = vertex_position;
    vec2 base;
    if (v.y <= 0)
    {
        v.y *= thickness;
        z = a.z;
        base = vec2(0, 0);
    }
    else
    {
        v.y = (v.y - 1.0f) * thickness + scale;
        z = b.z;
        base = vec2(0, scale);
    }
    v.x *= thickness;

    float angle = atan(delta.x, delta.y);

    mat2 rot = mat2(cos(angle), -sin(angle),
                    sin(angle), cos(angle));

    base = rot * base + a.xy;
    v = rot * v + a.xy;

    if (aspect > 1.0f)
    {
        v.y = (v.y - base.y) / aspect + base.y;
    }
    else
    {
        v.x = (v.x - base.x) / aspect + base.x;
    }

    gl_Position = vec4(v, z, 1.0f);
}

