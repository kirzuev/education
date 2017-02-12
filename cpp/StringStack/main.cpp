#include <iostream>
#include "string_stack.h"
#include <unistd.h>
#include <sys/wait.h>
#include <fcntl.h>

using namespace std;

bool test1 ()
{
	stack st;
	int i,l;

	try {
		for (i = 0; i < 27; i++) st.push("aaaa");
		if ((l = st.length()) == 27) return 1;
		cerr << "Test #1 is failed:" << endl;
		cerr << "\t***Expected: 27" << endl;
		cerr << "\t***But length() returned " << l << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #1 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test2 ()
{
	stack st;
	int i;

	try {
		for (i = 0; i < max_size; i++) st.push("aaaa");
		if (st.isfull()) return 1;
		cerr << "Test #2 is failed:" << endl;
		cerr << "\t***Expected: 1" << endl;
		cerr << "\t***But isfull() returned 0" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #2 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test3 ()
{
	stack st;
	int i;

	try {
		for (i = 0; i < max_size-1; i++) st.push("aaaa");
		if (!st.isfull()) return 1;
		cerr << "Test #3 is failed:" << endl;
		cerr << "\t***Expected: 0" << endl;
		cerr << "\t***But isfull() returned 1" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #3 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test4 ()
{
	stack st;

	try {
		st.push("aaaa");
		if (!st.isempty()) return 1;
		cerr << "Test #4 is failed:" << endl;
		cerr << "\t***Expected: 0" << endl;
		cerr << "\t***But isempty() returned 1" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #4 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test5 ()
{
	stack st;

	if (st.isempty()) return 1;
	cerr << "Test #5 is failed:" << endl;
	cerr << "\t***Expected: 1" << endl;
	cerr << "\t***But isempty() returned 0" << endl;
	return 0;
}

bool test6 ()
{
	stack st;
	int k = st.maxsize();

	if (k == max_size) return 1;
	cerr << "Test #6 is failed:" << endl;
	cerr << "\t***Expected: " << max_size << endl;
	cerr << "\t***But maxsize() returned " << k << endl;
	return 0;
}

bool test7 ()
{
	stack st;
	int i,l;

	try {
		for (i = 0; i < max_size; i++) st.push("aaaa");
		if ((l = st.length()) == max_size) return 1;
		cerr << "Test #7 is failed:" << endl;
		cerr << "\t***Expected: " << max_size << endl;
		cerr << "\t***But length() returned " << l << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #7 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test8 ()
{
	stack st;
	int l;

	if ((l = st.length()) == 0) return 1;
	cerr << "Test #8 is failed:" << endl;
	cerr << "\t***Expected: 0" << endl;
	cerr << "\t***But length() returned " << l << endl;
	return 0;
}

bool test9 ()
{
	stack st1, st2;

	if (st1 == st2) return 1;
	cerr << "Test #9 is failed:" << endl;
	cerr << "\t***Expected: 1" << endl;
	cerr << "\t***But operator== returned 0" << endl;
	return 0;
}

bool test10 ()
{
	stack st1, st2;

	try {
		st1.push("aaa"); st1.push("bbb"); st1.push("ccc");
		st2.push("aaa"); st2.push("bbb"); st2.push("ccc");
		if (st1 == st2) return 1;
		cerr << "Test #10 is failed:" << endl;
		cerr << "\t***Expected: 1" << endl;
		cerr << "\t***But operator== returned 0" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #10 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test11 ()
{
	stack st1, st2;
	try {
		st1.push("aaa"); st1.push("bbb"); st1.push("ccc");
		st2.push("aaa"); st2.push("bbb");
		if (st1 != st2) return 1;
		cerr << "Test #11 is failed:" << endl;
		cerr << "\t***Expected: 1" << endl;
		cerr << "\t***But operator!= returned 0" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #11 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test12 ()
{
	stack st1, st2;

	try {
		st1.push("aaa"); st1.push("bbb"); st1.push("ccc");
		st2.push("aaa"); st2.push("bbbb"); st2.push("ccc");
		if (st1 != st2) return 1;
		cerr << "Test #12 is failed:" << endl;
		cerr << "\t***Expected: 1" << endl;
		cerr << "\t***But operator!= returned 0" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #12 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test13 ()
{
	stack st;

	try {
		st.push("aaa"); st.push("bbb"); st.push("ccc");
		char* s = st[1];
		if (strcmp(s,"bbb") == 0) return 1;
		cerr << "Test #13 is failed:" << endl;
		cerr << "\t***Expected: 'bbb'" << endl;
		cerr << "\t***But operator[] returned '" << s << "'" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #13 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test14 ()
{
	stack st;

	try {
		st.push("aaa"); st.push("bbb"); st.push("ccc");
		char* s = (char*) st;
		if (strcmp(s,"cccbbbaaa") == 0) return 1;
		cerr << "Test #14 is failed:" << endl;
		cerr << "\t***Expected: 'cccbbbaaa'" << endl;
		cerr << "\t***But operator(char*) returned '" << s << "'" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #14 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test15 ()
{
	stack st;

	try {
		char* s = (char*) st;
		if (strcmp(s,"") == 0) return 1;
		cerr << "Test #15 is failed:" << endl;
		cerr << "\t***Expected: exception Empty_Stack()" << endl;
		cerr << "\t***But operator(char*) returned '" << s << "'" << endl;
		return 0;
	}
	catch (Stack_Err& p) { return 1; }
}

bool test16 ()
{
	stack st;

	try {
		st.push("aaa"); st.push("bbb"); st.push("ccc");
		char* s = st.peek();
		if (strcmp(s,"ccc") == 0) return 1;
		cerr << "Test #16 is failed:" << endl;
		cerr << "\t***Expected: 'ccc'" << endl;
		cerr << "\t***But peek() returned '" << s << "'" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #16 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test17 ()
{
	stack st;

	try {
		st.push("aaa"); st.push("bbb"); st.push("ccc");
		char* s1 = st.pop();
		char* s2 = st.pop();
		char* s3 = st.pop();
		if (strcmp(s1,"ccc") == 0 && strcmp(s2,"bbb") == 0 && strcmp(s3,"aaa") == 0) return 1;
		cerr << "Test #17 is failed:" << endl;
		cerr << "\t***Expected: 'ccc', 'bbb', 'aaa'" << endl;
		cerr << "\t***But pop() returned '" << s1 << "', '" << s2 << "', '" << s3 << "'" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #17 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test18 ()
{
	stack st;
	char *s1, *s2, *s3;

	try {
		st+"aaa"; st+"bbb"; st+"ccc";
		st-s1; st-s2; st-s3;
		if (strcmp(s1,"ccc") == 0 && strcmp(s2,"bbb") == 0 && strcmp(s3,"aaa") == 0) return 1;
		cerr << "Test #18 is failed:" << endl;
		cerr << "\t***Expected: 'ccc', 'bbb', 'aaa'" << endl;
		cerr << "\t***But operator- returned '" << s1 << "', '" << s2 << "', '" << s3 << "'" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #18 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test19 ()
{
	stack st;

	try {
		char* s = st.pop();
		cerr << "Test #19 is failed:" << endl;
		cerr << "\t***Expected: exception Empty_Stack()" << endl;
		cerr << "\t***But pop() returned '" << s << "'" << endl;
		return 0;
	}
	catch (Stack_Err&) { return 1; }
}

bool test20 ()
{
	stack st;

	try {
		char* s = st.peek();
		cerr << "Test #20 is failed:" << endl;
		cerr << "\t***Expected: exception Empty_Stack()" << endl;
		cerr << "\t***But peek() returned '" << s << "'" << endl;
		return 0;
	}
	catch (Stack_Err&) { return 1; }
}

bool test21 ()
{
	stack st;
	int i;

	try {
		for (i = 0; i < max_size + 1; i++) st.push("aaa");
		cerr << "Test #21 is failed:" << endl;
		cerr << "\t***Expected: exception Full_Stack()" << endl;
		cerr << "\t***But push() has worked correctly" << endl;
		return 0;
	}
	catch (Stack_Err&) { return 1; }
}

bool test22 ()
{
	stack st;
	int i;

	try {
		for (i = 0; i < max_size + 1; i++) st+"aaa";
		cerr << "Test #22 is failed:" << endl;
		cerr << "\t***Expected: exception Full_Stack()" << endl;
		cerr << "\t***But operator+ has worked correctly" << endl;
		return 0;
	}
	catch (Stack_Err&) { return 1; }
}

bool test23 ()
{
	stack st;
	char* s;

	try {
		st - s;
		cerr << "Test #23 is failed:" << endl;
		cerr << "\t***Expected: exception Empty_Stack()" << endl;
		cerr << "\t***But operator- returned '" << s << "'" << endl;
		return 0;
	}
	catch (Stack_Err&) { return 1; }
}

bool test24 ()
{
	stack st;

	try {
		st.push("aaa"); st.push("bbb"); st.push("ccc");
		char* s = st[3];
		cerr << "Test #13 is failed:" << endl;
		cerr << "\t***Expected: exception Wrong_Index()" << endl;
		cerr << "\t***But operator[] returned '" << s << "'" << endl;
		return 0;
	}
	catch (Stack_Err&) { return 1; }
}

bool test25 ()
{
	stack st;

	try {
		st.print();
		cerr << "Test #25 is failed:" << endl;
		cerr << "\t***Expected: exception Empty_Stack()" << endl;
		cerr << "\t***But print() has worked correctly" << endl;
		return 0;
	}
	catch (Stack_Err&) { return 1; }
}

bool test26 ()
{
	stack st1, st2;

	try {
		st1.push("aaa"); st1.push("bbb"); st1.push("ccc");
		st2 = st1;
		if (st1 == st2) return 1;
		cerr << "Test #26 is failed:" << endl;
		cerr << "\t***Expected: st1 == st2" << endl;
		cerr << "\t***But operator= doesn't work correctly" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #26 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test27 ()
{
	stack st1;

	try {
		st1.push("aaa"); st1.push("bbb"); st1.push("ccc");
		stack st2(st1);
		if (st1 == st2) return 1;
		cerr << "Test #27 is failed:" << endl;
		cerr << "\t***Expected: st1 == st2" << endl;
		cerr << "\t***But stack(const stack&) doesn't work correctly" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #27 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test28 ()
{
	stack st1, st2;

	try {
		st1.push("aaa"); st1.push("bbb"); st1.push("ccc");
		st2 = st1;
		if (strcmp((char*)st1, (char*)st2) == 0) return 1;
		cerr << "Test #28 is failed:" << endl;
		cerr << "\t***Expected: st1 == st2" << endl;
		cerr << "\t***But operator= doesn't work correctly" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #28 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test29 ()
{
	stack st1;

	try {
		st1.push("aaa"); st1.push("bbb"); st1.push("ccc");
		stack st2(st1);
		if (strcmp((char*)st1, (char*)st2) == 0) return 1;
		cerr << "Test #29 is failed:" << endl;
		cerr << "\t***Expected: st1 == st2" << endl;
		cerr << "\t***But stack(const stack&) doesn't work correctly" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #29 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test30 ()
{
	stack st1, st2;

	try {
		st1.push("aaa"); st1.push("bbb");
		st2 = st1;
		if (strcmp(st1[0], st2[0]) == 0 && strcmp(st1[1], st2[1]) == 0) return 1;
		cerr << "Test #30 is failed:" << endl;
		cerr << "\t***Expected: st1 == st2" << endl;
		cerr << "\t***But operator= doesn't work correctly" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #30 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test31 ()
{
	stack st1;

	try {
		st1.push("aaa"); st1.push("bbb");
		stack st2(st1);
		if (strcmp(st1[0], st2[0]) == 0 && strcmp(st1[1], st2[1]) == 0) return 1;
		cerr << "Test #31 is failed:" << endl;
		cerr << "\t***Expected: st1 == st2" << endl;
		cerr << "\t***But stack(const stack&) doesn't work correctly" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #31 is failed:" << endl;
		p.print();
	}
	return 0;
}

bool test32 ()
{
	stack st;
	int i;

	try {
		for (i = 0; i < max_size/4; i++) st.push("bbb");
		for (i = 0; i < max_size/2; i++) st.push("aaaa");
		for (i = 0; i < max_size/4; i++) st.push("ccccc");
		int fd = open("_test.txt", O_WRONLY|O_CREAT|O_TRUNC, 0666);
		int f0 = dup(1);
		int s;
		dup2(fd,1);
		close(fd);
		st.print();
		dup2(f0,1);
		if (!fork())
		{
			execlp("cmp", "cmp", "_test.txt", "test.txt", NULL);
			cerr << "Test #32 is failed:" << endl;
			return 0;
		}
		wait(&s);
		if (WIFEXITED(s) && WEXITSTATUS(s) == 0) return 1;
		cerr << "Test #32 is failed:" << endl;
		cerr << "Expected: *see on the file 'test.txt'*" << endl;
		cerr << "But print() printed: *see on the file '_test.txt'*" << endl;
	}
	catch (Stack_Err& p)
	{
		cerr << "Test #32 is failed:" << endl;
		p.print();
	}
	return 0;
}

int main ()
{
	int test = 0;
	test += test1();
	test += test2();
	test += test3();
	test += test4();
	test += test5();
	test += test6();
	test += test7();
	test += test8();
	test += test9();
	test += test10();
	test += test11();
	test += test12();
	test += test13();
	test += test14();
	test += test15();
	test += test16();
	test += test17();
	test += test18();
	test += test19();
	test += test20();
	test += test21();
	test += test22();
	test += test23();
	test += test24();
	test += test25();
	test += test26();
	test += test27();
	test += test28();
	test += test29();
	test += test30();
	test += test31();
	test += test32();
	cout << test << "/32 tests completed" << endl;
	return 0;
}
