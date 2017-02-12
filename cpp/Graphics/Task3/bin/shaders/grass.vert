#version 330

in vec4 point;
in vec2 position;
in vec4 variance;
in vec2 texCoord;

out vec2 TexCoord;
out vec4 color;

uniform mat4 camera;

void main() {
    float s = (gl_InstanceID % 15) / 14.0;
    float c = sqrt(1 - s*s);
    mat4 rotMatrix = mat4(0.0);
    rotMatrix[1][1] = 1;
    rotMatrix[0][0] = c;
    rotMatrix[2][2] = c;
    rotMatrix[0][2] = s;
    rotMatrix[2][0] = -s;
    rotMatrix[3][3] = 1;
    mat4 scaleMatrix = mat4(1.0 + (gl_InstanceID % 8) / 14.0);
    scaleMatrix[0][0] = 0.1;
    scaleMatrix[1][1] = 0.1;
    scaleMatrix[2][2] = 0.1;
    mat4 positionMatrix = mat4(1.0);
    positionMatrix[3][0] = position.x;
    positionMatrix[3][2] = position.y;
    vec4 newpoint = point * rotMatrix;
    float red;
	float green = (gl_InstanceID % 8) / 28.0 + newpoint.y;	
	if (newpoint.y > 0.6) red = 1.6;
		else red = 0.2;
	color = vec4(red, green, 0, 0);
	TexCoord = texCoord;
    gl_Position = camera * (positionMatrix * scaleMatrix * newpoint + variance * (newpoint.y * newpoint.y));

}
