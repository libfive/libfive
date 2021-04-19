#version 150
#extension GL_ARB_explicit_attrib_location : require

layout (triangles) in;
layout (triangle_strip, max_vertices = 3) out;

uniform mat4 M;

vec3 normal(vec3 p1, vec3 p2, vec3 p3)
{
   vec3 a = p2 - p1;
   vec3 b = p3 - p1;
   return normalize(cross(a, b));
}

void main()
{
    //Generate normal from triangle
    vec3 norm = normal(gl_in[0].gl_Position.xyz, gl_in[1].gl_Position.xyz, gl_in[2].gl_Position.xyz);

    //Offset to 'thicken' the object
    float offset = 0.05;
    norm = norm*offset;

    //Apply to each vertex of the triangle
    gl_Position = M*(gl_in[0].gl_Position + vec4(norm,0));
    EmitVertex();

    gl_Position = M*(gl_in[1].gl_Position + vec4(norm,0));
    EmitVertex();

    gl_Position = M*(gl_in[2].gl_Position + vec4(norm,0));
    EmitVertex();
    
    EndPrimitive();
}  
