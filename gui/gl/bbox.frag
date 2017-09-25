#version 330

uniform float scale;

in vec3 frag_bary;
out vec4 fragColor;

void main()
{
    float cutoff = pow(0.995f, 1/scale);
    float edge = max(frag_bary.r + frag_bary.g, frag_bary.r + frag_bary.b);
    if (edge > cutoff)
    {
        fragColor = vec4(1.0f, 1.0f, 1.0f, 1.0f);
    }
    else
    {
        discard;
    }
}
