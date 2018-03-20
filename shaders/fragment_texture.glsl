#version 330 core

out vec4 color;

in vec3 f_color;
in vec3 f_normal;
in vec2 f_uv;
uniform sampler2D T0;
uniform vec3 L_dir;
uniform vec3 L_col;
uniform vec3 A;

void main() {
  float spot_brightness = dot(L_dir, normalize(f_normal));
  vec3  lighting = spot_brightness * L_col + A;
  vec4  tex = texture(T0, f_uv);
  color.rgb = lighting * f_color * tex.rgb;
  color.a = tex.a;
 }