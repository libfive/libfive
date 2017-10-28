#version 330

in vec3 ec_pos;
in vec3 frag_norm;
in float frag_sharp;

uniform vec3 shade;

out vec4 fragColor;

void main() {
    vec3 base3 = vec3(0.99, 0.96, 0.89) * shade;
    vec3 base02 = vec3(0.03, 0.21, 0.26) * shade;

    // Per-face flat normals
    vec3 ec_normal = cross(dFdx(ec_pos), dFdy(ec_pos));
    ec_normal.z /= 8;
    ec_normal = -normalize(ec_normal);

    // Mix per-face flat normal with per-vertex smooth normals
    // based on how sharp this vertex should be.
    vec3 n;
    if (length(frag_norm) > 0.0f)
    {
        float f = min(1.0f, max(0.0f, (frag_sharp - 0.9f) * 10.0f));
        n = normalize(ec_normal * f + frag_norm * (1.0f - f));
    }
    else
    {
        n = normalize(ec_normal);
    }

    // Per-fragment shading
    vec3 dpos = normalize(vec3(1.0, -1.0, 4.0) - ec_pos);
    float brightness = dot(n, dpos);
    fragColor = vec4(brightness * base3 + (1 - brightness) * base02, 1.0);

    fragColor = vec4(n, 1.0f);
}
