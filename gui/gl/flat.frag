#version 330

in vec3 frag_color;
out vec4 fragColor;

void main()
{
    fragColor = vec4(frag_color, 1.0f);
}
