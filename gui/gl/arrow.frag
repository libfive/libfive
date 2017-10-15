#version 330

uniform vec4 shade;
in vec3 frag_norm;
in vec3 frag_pos;

out vec4 fragColor;

void main() {
    vec3 base3 = shade.rgb;
    vec3 base02 = 0.5 * shade.rgb;

    // Per-fragment shading
    vec3 dpos = normalize(vec3(1.0, -1.0, 4.0) - frag_pos);
    float brightness = dot(frag_norm, dpos);

    //fragColor = vec4(frag_norm.xy, -frag_norm.z, 1.0f);
    fragColor = vec4(brightness * base3 + (1 - brightness) * base02, shade.a);
}

