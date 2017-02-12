#include <iostream>
#include <vector>

#include "Texture.h"
#include "SOIL.h"
#include "Utility.h"

using namespace std;

const uint GRASS_INSTANCES = 10000; // Количество травинок
const uint BIRD_INSTANCES = 8; // Количество птиц
const uint ROCK_INSTANCES = 5; // Количество камней

GL::Camera camera;               // Мы предоставляем Вам реализацию камеры. В OpenGL камера - это просто 2 матрицы. Модельно-видовая матрица и матрица проекции. // ###
                                 // Задача этого класса только в том чтобы обработать ввод с клавиатуры и правильно сформировать эти матрицы.
                                 // Вы можете просто пользоваться этим классом для расчёта указанных матриц.


GLuint grassPointsCount; // Количество вершин у модели травинки
GLuint grassShader;      // Шейдер, рисующий траву
GLuint grassVAO;         // VAO для травы (что такое VAO почитайте в доках)
GLuint grassVariance;    // Буфер для смещения координат травинок
GLuint grassTexture;
vector<VM::vec4> grassVarianceData(GRASS_INSTANCES); // Вектор со смещениями для координат травинок

GLuint groundShader; // Шейдер для земли
GLuint groundVAO; // VAO для земли
GLuint groundTexture;

GLuint skyShader;
GLuint skyVAO;
GLuint skyTexture;

GLuint birdShader;
GLuint birdVAO;
GLuint birdTexture;

GLuint rockShader;
GLuint rockVAO;
GLuint rockTexture;

// Размеры экрана
uint screenWidth = 800;
uint screenHeight = 600;

// Это для захвата мышки. Вам это не потребуется (это не значит, что нужно удалять эту строку)
bool captureMouse = true;

bool isAA = true;
bool blowing = true;

// Функция, рисующая землю
void DrawGround() {
    // Используем шейдер для земли
    glUseProgram(groundShader);                                                  CHECK_GL_ERRORS

    // Устанавливаем юниформ для шейдера. В данном случае передадим перспективную матрицу камеры
    // Находим локацию юниформа 'camera' в шейдере
    GLint cameraLocation = glGetUniformLocation(groundShader, "camera");         CHECK_GL_ERRORS
    // Устанавливаем юниформ (загружаем на GPU матрицу проекции?)                                                     // ###
    glUniformMatrix4fv(cameraLocation, 1, GL_TRUE, camera.getMatrix().data().data()); CHECK_GL_ERRORS

    // Подключаем VAO, который содержит буферы, необходимые для отрисовки земли
    glBindVertexArray(groundVAO);                                                CHECK_GL_ERRORS

	GL::bindTexture(groundShader, "ourTexture", groundTexture, 0);                                CHECK_GL_ERRORS

    // Рисуем землю: 2 треугольника (6 вершин)
    glDrawArrays(GL_TRIANGLES, 0, 6);                                            CHECK_GL_ERRORS

    // Отсоединяем VAO
    glBindVertexArray(0);                                                        CHECK_GL_ERRORS
    // Отключаем шейдер
    glUseProgram(0);                                                             CHECK_GL_ERRORS
}

void DrawSky() {
	glDepthFunc(GL_LEQUAL);                                                   CHECK_GL_ERRORS
    glUseProgram(skyShader);                                                  CHECK_GL_ERRORS

    GLint cameraLocation = glGetUniformLocation(groundShader, "camera");         CHECK_GL_ERRORS
    glUniformMatrix4fv(cameraLocation, 1, GL_TRUE, camera.getMatrix().data().data()); CHECK_GL_ERRORS

    glBindVertexArray(skyVAO);                                                CHECK_GL_ERRORS
	glActiveTexture(GL_TEXTURE0);                                             CHECK_GL_ERRORS

	glUniform1i(glGetUniformLocation(skyShader, "skybox"), 0);                CHECK_GL_ERRORS
	glBindTexture(GL_TEXTURE_CUBE_MAP, skyTexture);                           CHECK_GL_ERRORS

    glDrawArrays(GL_TRIANGLES, 0, 36);                                        CHECK_GL_ERRORS

    glBindVertexArray(0);                                                     CHECK_GL_ERRORS
    glUseProgram(0);                                                          CHECK_GL_ERRORS
	glDepthFunc(GL_LESS);                                        			  CHECK_GL_ERRORS
}

// Обновление смещения травинок
void UpdateGrassVariance() {
    // Генерация случайных смещений
	double a = M_PI * 0.0007 * glutGet(GLUT_ELAPSED_TIME);
    for (uint i = 0; i < GRASS_INSTANCES; ++i) {
		if (blowing)
		{
	        grassVarianceData[i].x = -0.02 * (0.7 + sin(a));
	        grassVarianceData[i].y = -0.02 * (0.7 + sin(a));
	        grassVarianceData[i].z = 0.02 * (0.7 + sin(a));
		}
		else
		{
 	    	grassVarianceData[i].x *= 0.9;
 	    	grassVarianceData[i].y *= 0.9;
    		grassVarianceData[i].z *= 0.9;
   		}
 	}

    // Привязываем буфер, содержащий смещения
    glBindBuffer(GL_ARRAY_BUFFER, grassVariance);                                CHECK_GL_ERRORS
    // Загружаем данные в видеопамять
    glBufferData(GL_ARRAY_BUFFER, sizeof(VM::vec4) * GRASS_INSTANCES, grassVarianceData.data(), GL_STATIC_DRAW); CHECK_GL_ERRORS
    // Отвязываем буфер
    glBindBuffer(GL_ARRAY_BUFFER, 0);                                            CHECK_GL_ERRORS
}


// Рисование травы
void DrawGrass() {
    // Тут то же самое, что и в рисовании земли
    glUseProgram(grassShader);                                                   CHECK_GL_ERRORS
    GLint cameraLocation = glGetUniformLocation(grassShader, "camera");          CHECK_GL_ERRORS
    glUniformMatrix4fv(cameraLocation, 1, GL_TRUE, camera.getMatrix().data().data()); CHECK_GL_ERRORS
    glBindVertexArray(grassVAO);                                                 CHECK_GL_ERRORS
    // Обновляем смещения для травы
    UpdateGrassVariance();
	
	GL::bindTexture(grassShader, "ourTexture", grassTexture, 0);   CHECK_GL_ERRORS

  	// Отрисовка травинок в количестве GRASS_INSTANCES
	glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, grassPointsCount, GRASS_INSTANCES);   CHECK_GL_ERRORS
	glBindVertexArray(0);                                                        CHECK_GL_ERRORS
    glUseProgram(0);                                                             CHECK_GL_ERRORS
}

void DrawBird() {
    glUseProgram(birdShader);                                                   CHECK_GL_ERRORS
    GLint cameraLocation = glGetUniformLocation(birdShader, "camera");          CHECK_GL_ERRORS
    glUniformMatrix4fv(cameraLocation, 1, GL_TRUE, camera.getMatrix().data().data()); CHECK_GL_ERRORS
    glBindVertexArray(birdVAO);                                                 CHECK_GL_ERRORS
	
	glDrawArraysInstanced(GL_TRIANGLES, 0, 18, BIRD_INSTANCES);   CHECK_GL_ERRORS
	glBindVertexArray(0);                                                        CHECK_GL_ERRORS
    glUseProgram(0);                                                             CHECK_GL_ERRORS
}

void DrawRock() {
    glUseProgram(rockShader);                                                   CHECK_GL_ERRORS
    GLint cameraLocation = glGetUniformLocation(rockShader, "camera");          CHECK_GL_ERRORS
    glUniformMatrix4fv(cameraLocation, 1, GL_TRUE, camera.getMatrix().data().data()); CHECK_GL_ERRORS
    glBindVertexArray(rockVAO);                                                 CHECK_GL_ERRORS
	
	glDrawArraysInstanced(GL_TRIANGLES, 0, 12, ROCK_INSTANCES);   CHECK_GL_ERRORS
	glBindVertexArray(0);                                                        CHECK_GL_ERRORS
    glUseProgram(0);                                                             CHECK_GL_ERRORS
}

// Эта функция вызывается для обновления экрана
void RenderLayouts() {
    // Включение буфера глубины
    glEnable(GL_DEPTH_TEST);
    // Очистка буфера глубины и цветового буфера
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    // Рисуем меши
    DrawSky();  
    DrawGround();
    DrawGrass();  
	DrawBird();
	DrawRock();
    glutSwapBuffers();
}

// Завершение программы
void FinishProgram() {
    glutDestroyWindow(glutGetWindow());
}

// Обработка события нажатия клавиши (специальные клавиши обрабатываются в функции SpecialButtons)
void KeyboardEvents(unsigned char key, int x, int y) {
    if (key == 27) {
        FinishProgram();
    } else if (key == 'w') {
        camera.goForward();
    } else if (key == 's') {
        camera.goBack();
	} else if (key == 'b') {
		blowing = !blowing;
	} else if (key == 'a' && isAA) {
		glDisable(GL_MULTISAMPLE);                                                CHECK_GL_ERRORS
		isAA = !isAA;
	} else if (key == 'a' && !isAA) {
		glEnable(GL_MULTISAMPLE);                                                 CHECK_GL_ERRORS
		isAA = !isAA;
    } else if (key == 'm') {
        captureMouse = !captureMouse;
        if (captureMouse) {
            glutWarpPointer(screenWidth / 2, screenHeight / 2);
            glutSetCursor(GLUT_CURSOR_NONE);
        } else {
            glutSetCursor(GLUT_CURSOR_RIGHT_ARROW);
        }
    }
}

// Обработка события нажатия специальных клавиш
void SpecialButtons(int key, int x, int y) {
    if (key == GLUT_KEY_RIGHT) {
        camera.rotateY(0.02);
    } else if (key == GLUT_KEY_LEFT) {
        camera.rotateY(-0.02);
    } else if (key == GLUT_KEY_UP) {
        camera.rotateTop(-0.02);
    } else if (key == GLUT_KEY_DOWN) {
        camera.rotateTop(0.02);
    }
}

void IdleFunc() {
    glutPostRedisplay();
}

// Обработка события движения мыши
void MouseMove(int x, int y) {
    if (captureMouse) {
        int centerX = screenWidth / 2,
            centerY = screenHeight / 2;
        if (x != centerX || y != centerY) {
            camera.rotateY((x - centerX) / 1000.0f);
            camera.rotateTop((y - centerY) / 1000.0f);
            glutWarpPointer(centerX, centerY);
        }
    }
}

// Обработка нажатия кнопки мыши
void MouseClick(int button, int state, int x, int y) {
}

// Событие изменение размера окна
void windowReshapeFunc(GLint newWidth, GLint newHeight) {
    glViewport(0, 0, newWidth, newHeight);
    screenWidth = newWidth;
    screenHeight = newHeight;

    camera.screenRatio = (float)screenWidth / screenHeight;
}

// Инициализация окна
void InitializeGLUT(int argc, char **argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH | GLUT_MULTISAMPLE);
	glEnable(GL_MULTISAMPLE);                                                                 CHECK_GL_ERRORS
    glutInitContextVersion(3, 0);
    glutInitWindowPosition(-1, -1);
    glutInitWindowSize(screenWidth, screenHeight);
    glutCreateWindow("Computer Graphics 3");
    glutWarpPointer(400, 300);
    glutSetCursor(GLUT_CURSOR_NONE);

    glutDisplayFunc(RenderLayouts);
    glutKeyboardFunc(KeyboardEvents);
    glutSpecialFunc(SpecialButtons);
    glutIdleFunc(IdleFunc);
    glutPassiveMotionFunc(MouseMove);
    glutMouseFunc(MouseClick);
    glutReshapeFunc(windowReshapeFunc);
}

// Генерация позиций травинок (эту функцию вам придётся переписать)
vector<VM::vec2> GenerateGrassPositions() {
    vector<VM::vec2> grassPositions(GRASS_INSTANCES);
	for (uint i = 0; i < GRASS_INSTANCES; ++i) {
		grassPositions[i] = VM::vec2(0.01 + 1.98 * rand() / double(RAND_MAX),
									 0.01 + 1.98 * rand() / double(RAND_MAX));
  	}
    return grassPositions;
}

vector<VM::vec2> GenerateBirdPositions() {
	vector<VM::vec2> birdPositions(BIRD_INSTANCES);
	for (uint i = 0; i < BIRD_INSTANCES; ++i) {
		birdPositions[i] = VM::vec2(0.1 + 1.8 * rand() / double(RAND_MAX),
									0.1 + 1.8 * rand() / double(RAND_MAX));
	}
	return birdPositions;
}

vector<VM::vec2> GenerateRockPositions() {
	vector<VM::vec2> rockPositions(ROCK_INSTANCES);
	for (uint i = 0; i < ROCK_INSTANCES; ++i) {
		rockPositions[i] = VM::vec2(0.1 + 1.8 * rand() / double(RAND_MAX),
									0.1 + 1.8 * rand() / double(RAND_MAX));
	}
	return rockPositions;
}

vector<VM::vec2> GenTexCoord() {
	return {
		VM::vec2(0, 1),
		VM::vec2(0, 0),
		VM::vec2(0.3, 1),
		VM::vec2(0.3, 0),
		VM::vec2(0.7, 1),
		VM::vec2(0.7, 0),
		VM::vec2(1, 0.5)
	};
}

// Здесь вам нужно будет генерировать меш
vector<VM::vec4> GenMesh(uint n) {
	return {
        VM::vec4(    0,   0, 0, 1), // 0
        VM::vec4( 0.05,   0, 0, 1), // 1
        VM::vec4(0.025, 0.3, 0, 1), // 2
		VM::vec4(0.075, 0.3, 0, 1), // 3
		VM::vec4(0.075, 0.6, 0, 1), // 4
		VM::vec4(0.125, 0.6, 0, 1), // 5
		VM::vec4( 0.15,   1, 0, 1)  // 6
	};
}

// Создание травы
void CreateGrass() {
    uint LOD = 1;
    // Создаём меш
    vector<VM::vec4> grassPoints = GenMesh(LOD);
    vector<VM::vec2> texCoord = GenTexCoord();
    // Сохраняем количество вершин в меше травы
    grassPointsCount = grassPoints.size();
    // Создаём позиции для травинок
    vector<VM::vec2> grassPositions = GenerateGrassPositions();
    // Инициализация смещений для травинок
    for (uint i = 0; i < GRASS_INSTANCES; ++i) {
        grassVarianceData[i] = VM::vec4(0, 0, 0, 0);
    }

    /* Компилируем шейдеры
    Эта функция принимает на вход название шейдера 'shaderName',
    читает файлы shaders/{shaderName}.vert - вершинный шейдер
    и shaders/{shaderName}.frag - фрагментный шейдер,
    компилирует их и линкует.
    */
    grassShader = GL::CompileShaderProgram("grass");

    // Здесь создаём буфер
    GLuint pointsBuffer;
    // Это генерация одного буфера (в pointsBuffer хранится идентификатор буфера)
    glGenBuffers(1, &pointsBuffer);                                              CHECK_GL_ERRORS
    // Привязываем сгенерированный буфер
    glBindBuffer(GL_ARRAY_BUFFER, pointsBuffer);                                 CHECK_GL_ERRORS
    // Заполняем буфер данными из вектора
    glBufferData(GL_ARRAY_BUFFER, sizeof(VM::vec4) * grassPoints.size(), grassPoints.data(), GL_STATIC_DRAW); CHECK_GL_ERRORS

  // Создание VAO
    // Генерация VAO
    glGenVertexArrays(1, &grassVAO);                                             CHECK_GL_ERRORS
    // Привязка VAO
    glBindVertexArray(grassVAO);                                                 CHECK_GL_ERRORS

   // Получение локации параметра 'point' в шейдере
    GLuint pointsLocation = glGetAttribLocation(grassShader, "point");           CHECK_GL_ERRORS
    // Подключаем массив атрибутов к данной локации
    glEnableVertexAttribArray(pointsLocation);                                   CHECK_GL_ERRORS
    // Устанавливаем параметры для получения данных из массива (по 4 значение типа float на одну вершину)
    glVertexAttribPointer(pointsLocation, 4, GL_FLOAT, GL_FALSE, 0, 0);          CHECK_GL_ERRORS

   // Создаём буфер для позиций травинок
    GLuint positionBuffer;
    glGenBuffers(1, &positionBuffer);                                            CHECK_GL_ERRORS
    // Здесь мы привязываем новый буфер, так что дальше вся работа будет с ним до следующего вызова glBindBuffer
    glBindBuffer(GL_ARRAY_BUFFER, positionBuffer);                               CHECK_GL_ERRORS
    glBufferData(GL_ARRAY_BUFFER, sizeof(VM::vec2) * grassPositions.size(), grassPositions.data(), GL_STATIC_DRAW); CHECK_GL_ERRORS

    GLuint positionLocation = glGetAttribLocation(grassShader, "position");      CHECK_GL_ERRORS
    glEnableVertexAttribArray(positionLocation);                                 CHECK_GL_ERRORS
    glVertexAttribPointer(positionLocation, 2, GL_FLOAT, GL_FALSE, 0, 0);        CHECK_GL_ERRORS
    // Здесь мы указываем, что нужно брать новое значение из этого буфера для каждого инстанса (для каждой травинки)
    glVertexAttribDivisor(positionLocation, 1);                                  CHECK_GL_ERRORS

    // Создаём буфер для смещения травинок
    glGenBuffers(1, &grassVariance);                                            CHECK_GL_ERRORS
    glBindBuffer(GL_ARRAY_BUFFER, grassVariance);                               CHECK_GL_ERRORS
    glBufferData(GL_ARRAY_BUFFER, sizeof(VM::vec4) * GRASS_INSTANCES, grassVarianceData.data(), GL_STATIC_DRAW); CHECK_GL_ERRORS

    GLuint varianceLocation = glGetAttribLocation(grassShader, "variance");      CHECK_GL_ERRORS
    glEnableVertexAttribArray(varianceLocation);                                 CHECK_GL_ERRORS
    glVertexAttribPointer(varianceLocation, 4, GL_FLOAT, GL_FALSE, 0, 0);        CHECK_GL_ERRORS
    glVertexAttribDivisor(varianceLocation, 1);                                  CHECK_GL_ERRORS

    GLuint texCoordBuffer;
    glGenBuffers(1, &texCoordBuffer);                                              CHECK_GL_ERRORS
    glBindBuffer(GL_ARRAY_BUFFER, texCoordBuffer);                                 CHECK_GL_ERRORS
    glBufferData(GL_ARRAY_BUFFER, sizeof(VM::vec2) * texCoord.size(), texCoord.data(), GL_STATIC_DRAW); CHECK_GL_ERRORS

   GLuint texCoordLocation = glGetAttribLocation(grassShader, "texCoord");           CHECK_GL_ERRORS
    glEnableVertexAttribArray(texCoordLocation);                                   CHECK_GL_ERRORS
    glVertexAttribPointer(texCoordLocation, 2, GL_FLOAT, GL_FALSE, 0, 0);          CHECK_GL_ERRORS

// Загрузка текстуры
	glGenTextures(1, &grassTexture);                 CHECK_GL_ERRORS
	glBindTexture(GL_TEXTURE_2D, grassTexture);                 CHECK_GL_ERRORS
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);                 CHECK_GL_ERRORS
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);                 CHECK_GL_ERRORS
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);                 CHECK_GL_ERRORS
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);                 CHECK_GL_ERRORS
	int width, height;
	unsigned char* image = SOIL_load_image("../Texture/grass.jpg", &width, &height, 0, SOIL_LOAD_RGB);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, image);                 CHECK_GL_ERRORS
	glGenerateMipmap(GL_TEXTURE_2D);                 CHECK_GL_ERRORS
	SOIL_free_image_data(image);
	glBindTexture(GL_TEXTURE_2D, 0);                 CHECK_GL_ERRORS
	glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, 0);                 CHECK_GL_ERRORS

    // Отвязываем VAO
    glBindVertexArray(0);                                                        CHECK_GL_ERRORS
    // Отвязываем буфер
    glBindBuffer(GL_ARRAY_BUFFER, 0);                                            CHECK_GL_ERRORS
}

// Создаём камеру (Если шаблонная камера вам не нравится, то можете переделать, но я бы не стал)
void CreateCamera() {
    camera.angle = 45.0f / 180.0f * M_PI;
    camera.direction = VM::vec3(0, 0.3, -1);
    camera.position = VM::vec3(0.5, 0.2, 0);
    camera.screenRatio = (float)screenWidth / screenHeight;
    camera.up = VM::vec3(0, 1, 0);
    camera.zfar = 50.0f;
    camera.znear = 0.05f;
}

// Создаём землю
void CreateGround() {
    // Земля состоит из двух треугольников
    vector<VM::vec4> meshPoints = {
        VM::vec4( 0, 0,  0, 10),
        VM::vec4(20, 0,  0, 10),
        VM::vec4(20, 0, 20, 10),
        VM::vec4( 0, 0,  0, 10),
        VM::vec4(20, 0, 20, 10),
        VM::vec4( 0, 0, 20, 10),
    };

	groundShader = GL::CompileShaderProgram("ground");

    GLuint pointsBuffer;
    glGenBuffers(1, &pointsBuffer);                                              CHECK_GL_ERRORS
    glBindBuffer(GL_ARRAY_BUFFER, pointsBuffer);                                 CHECK_GL_ERRORS
    glBufferData(GL_ARRAY_BUFFER, sizeof(VM::vec4) * meshPoints.size(), meshPoints.data(), GL_STATIC_DRAW); CHECK_GL_ERRORS

    glGenVertexArrays(1, &groundVAO);                                            CHECK_GL_ERRORS
    glBindVertexArray(groundVAO);                                                CHECK_GL_ERRORS

    GLuint index = glGetAttribLocation(groundShader, "point");                   CHECK_GL_ERRORS
    glEnableVertexAttribArray(index);                                            CHECK_GL_ERRORS
    glVertexAttribPointer(index, 4, GL_FLOAT, GL_FALSE, 0, 0);                   CHECK_GL_ERRORS

// Загрузка текстуры
	glGenTextures(1, &groundTexture);                 CHECK_GL_ERRORS
	glBindTexture(GL_TEXTURE_2D, groundTexture);                 CHECK_GL_ERRORS
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_MIRRORED_REPEAT);                 CHECK_GL_ERRORS
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_MIRRORED_REPEAT);                 CHECK_GL_ERRORS
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);                 CHECK_GL_ERRORS
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);                 CHECK_GL_ERRORS
	int width, height;
	unsigned char* image = SOIL_load_image("../Texture/ground.jpg", &width, &height, 0, SOIL_LOAD_RGB);
 	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, image);                 CHECK_GL_ERRORS
	glGenerateMipmap(GL_TEXTURE_2D);                 CHECK_GL_ERRORS
	SOIL_free_image_data(image);
	glBindTexture(GL_TEXTURE_2D, 0);                 CHECK_GL_ERRORS

    glBindVertexArray(0);                                                        CHECK_GL_ERRORS
    glBindBuffer(GL_ARRAY_BUFFER, 0);                                            CHECK_GL_ERRORS
}

void CreateSky() {
	vector<VM::vec4> meshPoints = {
        VM::vec4(-2,  4, -2, 1),
        VM::vec4(-2, -2, -2, 1),
        VM::vec4( 4, -2, -2, 1),
        VM::vec4( 4, -2, -2, 1),
        VM::vec4( 4,  4, -2, 1),
        VM::vec4(-2,  4, -2, 1),
  
        VM::vec4(-2, -2,  4, 1),
        VM::vec4(-2, -2, -2, 1),
        VM::vec4(-2,  4, -2, 1),
        VM::vec4(-2,  4, -2, 1),
        VM::vec4(-2,  4,  4, 1),
        VM::vec4(-2, -2,  4, 1),
  
        VM::vec4( 4, -2, -2, 1),
        VM::vec4( 4, -2,  4, 1),
        VM::vec4( 4,  4,  4, 1),
        VM::vec4( 4,  4,  4, 1),
        VM::vec4( 4,  4, -2, 1),
        VM::vec4( 4, -2, -2, 1),
  
        VM::vec4(-2, -2,  4, 1),
        VM::vec4(-2,  4,  4, 1),
        VM::vec4( 4,  4,  4, 1),
        VM::vec4( 4,  4,  4, 1),
        VM::vec4( 4, -2,  4, 1),
        VM::vec4(-2, -2,  4, 1),
  
        VM::vec4(-2,  4, -2, 1),
        VM::vec4( 4,  4, -2, 1),
        VM::vec4( 4,  4,  4, 1),
        VM::vec4( 4,  4,  4, 1),
        VM::vec4(-2,  4,  4, 1),
        VM::vec4(-2,  4, -2, 1),
  
        VM::vec4(-2, -2, -2, 1),
        VM::vec4(-2, -2,  4, 1),
        VM::vec4( 4, -2, -2, 1),
        VM::vec4( 4, -2, -2, 1),
        VM::vec4(-2, -2,  4, 1),
        VM::vec4( 4, -2,  4, 1),
    };

	skyShader = GL::CompileShaderProgram("sky");

    GLuint pointsBuffer;
    glGenBuffers(1, &pointsBuffer);                                              CHECK_GL_ERRORS
    glBindBuffer(GL_ARRAY_BUFFER, pointsBuffer);                                 CHECK_GL_ERRORS
    glBufferData(GL_ARRAY_BUFFER, sizeof(VM::vec4) * meshPoints.size(), meshPoints.data(), GL_STATIC_DRAW); CHECK_GL_ERRORS

    glGenVertexArrays(1, &skyVAO);                                            CHECK_GL_ERRORS
    glBindVertexArray(skyVAO);                                                CHECK_GL_ERRORS

    GLuint index = glGetAttribLocation(skyShader, "point");                   CHECK_GL_ERRORS
    glEnableVertexAttribArray(index);                                            CHECK_GL_ERRORS
    glVertexAttribPointer(index, 4, GL_FLOAT, GL_FALSE, 0, 0);                   CHECK_GL_ERRORS

    vector<const GLchar*> pics;
    pics.push_back("../Texture/right.png");
    pics.push_back("../Texture/left.png");
    pics.push_back("../Texture/up.png");
    pics.push_back("../Texture/down.png");
    pics.push_back("../Texture/back.png");
    pics.push_back("../Texture/front.png");

	glGenTextures(1, &skyTexture);                 CHECK_GL_ERRORS
	int width, height;
	unsigned char* image;
	glBindTexture(GL_TEXTURE_CUBE_MAP, skyTexture);                 CHECK_GL_ERRORS

	for (GLuint i = 0; i < 6; i++)
	{
		image = SOIL_load_image(pics[i], &width, &height, 0, SOIL_LOAD_RGB);
		glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, image);
		SOIL_free_image_data(image);
	}
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
	glBindTexture(GL_TEXTURE_CUBE_MAP, 0);

    glBindVertexArray(0);                                                        CHECK_GL_ERRORS
    glBindBuffer(GL_ARRAY_BUFFER, 0);                                            CHECK_GL_ERRORS
}

void CreateBird() {
	vector<VM::vec4> meshPoints = {
        VM::vec4(-0.05, 0.31,     0, 1),
        VM::vec4(-0.03, 0.32,  0.01, 1),
        VM::vec4(-0.03, 0.32, -0.01, 1),

        VM::vec4(-0.03, 0.32,  0.01, 1),
        VM::vec4(-0.03, 0.32, -0.01, 1),
        VM::vec4(    0,  0.3, -0.01, 1),

        VM::vec4(    0,  0.3, -0.01, 1),
        VM::vec4(    0,  0.3,  0.01, 1),
        VM::vec4(-0.03, 0.32,  0.01, 1),

        VM::vec4(    0,  0.3, -0.01, 1),
        VM::vec4(    0,  0.3,  0.01, 1),
        VM::vec4( 0.03, 0.32,  0.01, 1),

        VM::vec4(    0,  0.3,  0.01, 1),
		VM::vec4( 0.03, 0.32, -0.01, 1),
        VM::vec4( 0.03, 0.32,  0.01, 1),

        VM::vec4( 0.03, 0.32, -0.01, 1),
        VM::vec4( 0.03, 0.32,  0.01, 1),
        VM::vec4( 0.05, 0.31,     0, 1),
    };
    vector<VM::vec2> birdPositions = GenerateBirdPositions();
	
	birdShader = GL::CompileShaderProgram("bird");
	
	GLuint pointsBuffer;
    glGenBuffers(1, &pointsBuffer);                                              CHECK_GL_ERRORS
    glBindBuffer(GL_ARRAY_BUFFER, pointsBuffer);                                 CHECK_GL_ERRORS
    glBufferData(GL_ARRAY_BUFFER, sizeof(VM::vec4) * meshPoints.size(), meshPoints.data(), GL_STATIC_DRAW); CHECK_GL_ERRORS

    glGenVertexArrays(1, &birdVAO);                                            CHECK_GL_ERRORS
    glBindVertexArray(birdVAO);                                                CHECK_GL_ERRORS

    GLuint index = glGetAttribLocation(birdShader, "point");                   CHECK_GL_ERRORS
    glEnableVertexAttribArray(index);                                            CHECK_GL_ERRORS
    glVertexAttribPointer(index, 4, GL_FLOAT, GL_FALSE, 0, 0);                   CHECK_GL_ERRORS

    GLuint positionBuffer;
    glGenBuffers(1, &positionBuffer);                                            CHECK_GL_ERRORS
    glBindBuffer(GL_ARRAY_BUFFER, positionBuffer);                               CHECK_GL_ERRORS
    glBufferData(GL_ARRAY_BUFFER, sizeof(VM::vec2) * birdPositions.size(), birdPositions.data(), GL_STATIC_DRAW); CHECK_GL_ERRORS

    GLuint positionLocation = glGetAttribLocation(birdShader, "position");      CHECK_GL_ERRORS
    glEnableVertexAttribArray(positionLocation);                                 CHECK_GL_ERRORS
    glVertexAttribPointer(positionLocation, 2, GL_FLOAT, GL_FALSE, 0, 0);        CHECK_GL_ERRORS
    glVertexAttribDivisor(positionLocation, 1);                                  CHECK_GL_ERRORS

    glBindVertexArray(0);                                                        CHECK_GL_ERRORS
    glBindBuffer(GL_ARRAY_BUFFER, 0);                                            CHECK_GL_ERRORS
}

void CreateRock() {
	vector<VM::vec4> meshPoints = {
        VM::vec4(-0.03,    0, -0.03, 1),
        VM::vec4(-0.03,    0,  0.03, 1),
        VM::vec4(    0, 0.05,     0, 1),

        VM::vec4(-0.03,    0,  0.03, 1),
        VM::vec4( 0.03,    0,  0.03, 1),
        VM::vec4(    0, 0.05,     0, 1),

        VM::vec4( 0.03,    0,  0.03, 1),
        VM::vec4( 0.03,    0, -0.03, 1),
        VM::vec4(    0, 0.05,     0, 1),

        VM::vec4(-0.03,    0, -0.03, 1),
        VM::vec4( 0.03,    0, -0.03, 1),
        VM::vec4(    0, 0.05,     0, 1),
   };
    vector<VM::vec2> rockPositions = GenerateRockPositions();
	
	rockShader = GL::CompileShaderProgram("rock");
	
	GLuint pointsBuffer;
    glGenBuffers(1, &pointsBuffer);                                              CHECK_GL_ERRORS
    glBindBuffer(GL_ARRAY_BUFFER, pointsBuffer);                                 CHECK_GL_ERRORS
    glBufferData(GL_ARRAY_BUFFER, sizeof(VM::vec4) * meshPoints.size(), meshPoints.data(), GL_STATIC_DRAW); CHECK_GL_ERRORS

    glGenVertexArrays(1, &rockVAO);                                            CHECK_GL_ERRORS
    glBindVertexArray(rockVAO);                                                CHECK_GL_ERRORS

    GLuint index = glGetAttribLocation(rockShader, "point");                   CHECK_GL_ERRORS
    glEnableVertexAttribArray(index);                                            CHECK_GL_ERRORS
    glVertexAttribPointer(index, 4, GL_FLOAT, GL_FALSE, 0, 0);                   CHECK_GL_ERRORS

    GLuint positionBuffer;
    glGenBuffers(1, &positionBuffer);                                            CHECK_GL_ERRORS
    glBindBuffer(GL_ARRAY_BUFFER, positionBuffer);                               CHECK_GL_ERRORS
    glBufferData(GL_ARRAY_BUFFER, sizeof(VM::vec2) * rockPositions.size(), rockPositions.data(), GL_STATIC_DRAW); CHECK_GL_ERRORS

    GLuint positionLocation = glGetAttribLocation(rockShader, "position");      CHECK_GL_ERRORS
    glEnableVertexAttribArray(positionLocation);                                 CHECK_GL_ERRORS
    glVertexAttribPointer(positionLocation, 2, GL_FLOAT, GL_FALSE, 0, 0);        CHECK_GL_ERRORS
    glVertexAttribDivisor(positionLocation, 1);                                  CHECK_GL_ERRORS

    glBindVertexArray(0);                                                        CHECK_GL_ERRORS
    glBindBuffer(GL_ARRAY_BUFFER, 0);                                            CHECK_GL_ERRORS
}

int main(int argc, char **argv)
{
    putenv("MESA_GL_VERSION_OVERRIDE=3.3COMPAT");
    try {
        cout << "Start" << endl;
        InitializeGLUT(argc, argv);
        cout << "GLUT inited" << endl;
        glewInit();
        cout << "glew inited" << endl;
        CreateCamera();
        cout << "Camera created" << endl;
        CreateSky();
        cout << "Sky created" << endl;
        CreateGrass();
        cout << "Grass created" << endl;
        CreateGround();
        cout << "Ground created" << endl;
        CreateRock();
        cout << "Rocks created" << endl;
        CreateBird();
        cout << "Birds created" << endl;
        glutMainLoop();
    } catch (string s) {
        cout << s << endl;
    }
}
