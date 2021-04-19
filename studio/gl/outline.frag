#version 150

in vec3 frag_norm;
in vec3 frag_pos;
in vec4 frag_color;

out vec4 fragColor;

void main() {
    //Render outline in black
    fragColor = vec4(0.0, 0.0, 0.0, 1.0);
}
