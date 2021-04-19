#version 150

uniform vec4 color_mul;
uniform vec4 color_add;
uniform int shading;

in vec3 frag_norm;
in vec3 frag_pos;
in vec4 frag_color;

out vec4 fragColor;

void main() {
    if (shading != 0)
    {
        vec3 norm;
        if (shading == 1)
        {
            norm = frag_norm;
        }
        else
        {
            norm = cross(dFdx(frag_pos), dFdy(frag_pos));
        }
        norm.z /= 8;
        norm = normalize(norm);

        float shininess = 16.0;

        // Per-fragment shading

        //Light positioning
        vec3 light_pos = vec3(0.0, 0.0, 1.0);
        
        //Diffuse lighting
        //z set to 3 to place the light further and get a more even diffuse lighting
        vec3 dpos = normalize(light_pos + vec3(frag_pos.xy,3));
        float diffuse = max(0.0, dot(norm, dpos));

        //Specular lighting
        vec3 spos = normalize(light_pos + vec3(frag_pos.xy,0));
        float specular = pow(max(0.0, dot(norm, spos)), shininess);

        //mix diffuse and specular
        float brightness = mix(diffuse, specular, 0.5); //50% mix

        fragColor = vec4(brightness, brightness, brightness, 1.0);
    }
    else
    {
        fragColor = frag_color;
    }
    fragColor = fragColor * color_mul + color_add;
}
