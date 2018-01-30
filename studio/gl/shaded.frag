#version 330

in vec3 frag_pos;
out vec4 fragColor;
uniform vec3 shade;

void main() {
    vec3 base3 = vec3(0.99, 0.96, 0.89) * shade;
    vec3 base02 = vec3(0.03, 0.21, 0.26) * shade;

    // Per-face flat normals
    vec3 frag_normal = cross(dFdx(frag_pos), dFdy(frag_pos));
    frag_normal.z /= 8;
    frag_normal = normalize(frag_normal);

    // Per-fragment shading
    vec3 dpos = normalize(vec3(1.0, -1.0, 4.0) - frag_pos);
    float brightness = dot(frag_normal, dpos);

    fragColor = vec4(brightness * base3 + (1 - brightness) * base02, 1.0);
}
