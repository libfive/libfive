#version 330

uniform vec4 frag_color;
out vec4 fragColor;

void main()
{
    fragColor = vec4(frag_color);
}
