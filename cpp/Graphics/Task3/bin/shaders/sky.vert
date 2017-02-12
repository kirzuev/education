#version 330

in vec4 point;

out vec3 texCoord;

uniform mat4 camera;

void main() {
	texCoord = vec3(point.x, point.y, point.z);
    gl_Position = camera * point;
}
