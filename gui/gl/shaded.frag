#version 330

uniform float zoom;
in vec3 ec_pos;
out vec4 fragColor;

void main() {
    vec3 base3 = vec3(0.99, 0.96, 0.89);
    vec3 base2 = vec3(0.92, 0.91, 0.83);
    vec3 base00 = vec3(0.40, 0.48, 0.51);

    vec3 ec_normal = normalize(cross(dFdx(ec_pos), dFdy(ec_pos)));
    ec_normal.z *= zoom;
    ec_normal = normalize(ec_normal);

    float a = dot(ec_normal, vec3(0.0, 0.0, 1.0));
    float b = dot(ec_normal, vec3(-0.57, -0.57, 0.57));

    fragColor = vec4((a*base2 + (1-a)*base00)*0.5 +
                     (b*base3 + (1-b)*base00)*0.5, 1.0);
}

