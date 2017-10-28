#version 330

uniform mat4 viewFromWorld;

uniform vec3 worldLightPos;
uniform bool flat;

in vec4 fragColor;
in vec3 worldFragNorm;
in vec4 colorBase;
in vec4 colorLight;

out vec4 outColor;

void main() {
    vec3 norm = normalize(flat
        ? invert(viewFromWorld) * cross(dFdx(gl_FragCoord.xyz), dFdy(gl_FragCoord.xyz))
        : worldFragNorm);

    // Per-fragment shading
    vec3 dpos = normalize(worldLightPos - invert(viewFromWorld) * gl_FragCoord);
    float brightness = dot(norm, dpos);

    outColor = brightness * fragColor * colorLight +
               (1 - brightness) * fragColor * colorBase;
}
