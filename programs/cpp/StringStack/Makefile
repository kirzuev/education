all: test

clean:
	rm test string_stack.o main.o _test.txt

string_stack.o: string_stack.cpp string_stack.h
	g++ -g -Wall -c string_stack.cpp

main.o: main.cpp
	g++ -g -Wall -c main.cpp

test: string_stack.o main.o
	g++ -g -Wall string_stack.o main.o -o test
