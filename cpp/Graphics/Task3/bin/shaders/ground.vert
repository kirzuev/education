#version 330

in vec4 point;

out vec2 texCoord;

uniform mat4 camera;

void main() {
	texCoord = vec2(point.x, point.z);
    gl_Position = camera * point;
}
