#version 330

uniform vec3 shade;

in vec3 frag_pos;
in vec3 frag_norm;

out vec4 fragColor;

void main() {
    vec3 base3 = vec3(0.99, 0.96, 0.89) * shade;
    vec3 base02 = vec3(0.03, 0.21, 0.26) * shade;

    // Per-fragment shading
    vec3 dpos = normalize(vec3(1.0, -1.0, 4.0) - frag_pos);
    float brightness = dot(frag_norm, dpos);

    fragColor = vec4(brightness * base3 + (1 - brightness) * base02, 1.0);
}
