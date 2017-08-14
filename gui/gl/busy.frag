#version 330

in vec3 frag_pos;
uniform float time;
uniform float fade;

out vec4 frag_color;

float pellet(vec2 pos)
{
  float y = abs(pos.y - 2.0);
  const float height = 0.5;

  if (y < height)
  {
      return abs(pos.x);
  }
  else
  {
      return sqrt(pow(pos.x, 2.0) + pow(y - height, 2.0));
  }
}

void main()
{
    vec2 pos = frag_pos.xy;

    float d = 1.0f;
    int count = 16;
    float c = float(count);
    for (int i=0; i < count; ++i)
    {
        float angle = float(i) / c * 2.0f * 3.14159;
        mat2 rot = mat2(cos(angle), sin(angle), -sin(angle), cos(angle));
        vec2 pos_ = rot * pos.xy * 3.0f;
        d = min(d, pellet(rot * pos.xy * 5.0f) -
                   mod(float(i) - time*c, c) / c * 0.1);
    }


    float r = 0.2;
    vec3 base = vec3(1.0, 1.0, 1.0);

    if (d < 0.0)
    {
        frag_color = vec4(1.0, 1.0, 1.0, fade);
    }
    else if (d < r)
    {
        float a = 1.0 - d/r;
        frag_color = vec4(a, a, a, a*fade);
    }
    else
    {
        discard;
    }
}

