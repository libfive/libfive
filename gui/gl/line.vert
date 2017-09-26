#version 330

layout(location=0) in vec2 vertex_position;

uniform vec3 a;
uniform vec3 b;
uniform float thickness;

void main()
{
    vec2 delta = (b - a).xy;
    float scale = length(delta);

    // Adjust line thickness
    float z = 0.0f;
    vec2 v = vertex_position;
    if (v.y < 0)
    {
        v.y *= thickness;
        z = a.z;
    }
    else
    {
        v.y = (v.y - 1.0f) * thickness + scale;
        z = b.z;
    }
    v.x *= thickness;

    float angle = atan(delta.x, delta.y);

    mat2 rot = mat2(cos(angle), -sin(angle),
                    sin(angle), cos(angle));

    gl_Position = vec4(rot * v + a.xy, z, 1.0f);
}

