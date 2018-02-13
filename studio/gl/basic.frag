#version 150

uniform vec4 color_mul;
uniform vec4 color_add;
uniform int shading;

in vec3 frag_norm;
in vec3 frag_pos;
in vec4 frag_color;

out vec4 fragColor;

void main() {
    if (shading != 0)
    {
        vec3 norm;
        if (shading == 1)
        {
            norm = frag_norm;
        }
        else
        {
            norm = cross(dFdx(frag_pos), dFdy(frag_pos));
        }
        norm.z /= 8;
        norm = normalize(norm);

        // Per-fragment shading
        vec3 dpos = normalize(vec3(1.0, -1.0, 4.0) - frag_pos);
        float brightness = dot(norm, dpos);

        fragColor = vec4(brightness, brightness, brightness, 1.0);
    }
    else
    {
        fragColor = frag_color;
    }
    fragColor = fragColor * color_mul + color_add;
}
