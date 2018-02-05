#version 330

in vec3 base_pos;
uniform float fade;

out vec4 frag_color;

float bar(vec2 pos)
{
  const float height = 0.04;
  const float width = 0.6;
  return max(
          max(pos.x - width, -width - pos.x),
          max(pos.y - height, -height - pos.y));
  /*
  if (y < height)
  {
      return abs(pos.x);
  }
  else
  {
      return sqrt(pow(pos.x, 2.0) + pow(y - height, 2.0));
  }
  */
}

void main()
{
    vec2 pos = base_pos.xy;

    float r = 0.05;
    vec3 base = vec3(1.0, 1.0, 1.0);

    float d = min(min(bar(pos), bar(vec2(pos.x, pos.y - 0.3))),
                  bar(vec2(pos.x, pos.y + 0.3)));
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


