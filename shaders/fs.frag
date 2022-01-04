#version 130

in vec4 fragPos;
in vec2 texPos;

uniform sampler2D tex;

out vec4 fragColor;

void main() {
  fragColor = texture(tex, texPos);
}
