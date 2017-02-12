#include <iostream>
#include "string_stack.h"

using namespace std;

void Empty_Stack::print()
{
	cerr << endl << "***STACK is EMPTY***" << endl;
	return;
}

void Full_Stack::print()
{
	cerr << endl << "***STACK is FULL***" << endl;
	return;
}

void Wrong_Index::print()
{
	cerr << endl << "***WRONG INDEX***" << endl;
	return;
}


void list::add (const char* s)
{
	node* p = new node;
	p->str = new char[strlen(s)+1];
	strcpy(p->str,s);
	p->next = head;
	head = p;
	return;
}

void list::remove ()
{
	node* p = head->next;
	delete head;
	head = p;
	return;
}

char* list::get () const
{
	char* s = new char[strlen(head->str)+1];
	strcpy(s,head->str);
	return s;
}

list::list(const list& l)
{
	if (l.head != NULL)
	{
		node* p = l.head;
		head = new node;
		head->str = new char[strlen(p->str)+1];
		strcpy(head->str,p->str);
		node* lst = head;
		lst->next = NULL;
		while (p->next != NULL) 		
		{
			lst->next = new node;
			lst = lst->next;
			p = p->next;
			lst->str = new char[strlen(p->str)+1];
			strcpy(lst->str,p->str);
			lst->next = NULL;
		}
	}
	else head = NULL;
}

list& list::operator = (const list& l)
{
	if (head == l.head) return *this;
	if (head != NULL) delete head;
	if (l.head != NULL)
	{
		node* p = l.head;
		head = new node;
		head->str = new char[strlen(p->str)+1];
		strcpy(head->str,p->str);
		node* lst = head;
		lst->next = NULL;
		while (p->next != NULL) 		
		{
			lst->next = new node;
			lst = lst->next;
			p = p->next;
			lst->str = new char[strlen(p->str)+1];
			strcpy(lst->str,p->str);
			lst->next = NULL;
		}
	}
	else head = NULL;
	return *this;
}
	

void stack::print () const throw (Stack_Err)
{
	if (l.head == NULL) throw Empty_Stack();
	node* p = l.head;
	cout << "===================" << endl;
	cout << "STACK:" << endl << endl;
	while (p != NULL)
	{
		cout << "*" << p->str << endl;
		p = p->next;
	}
	cout << "===================" << endl;
	return;
}

void stack::push (const char* s) throw (Stack_Err) 
{
	if (size == max_size) throw Full_Stack();
	l.add(s);
	size++;
	return;
}

char* stack::pop () throw (Stack_Err)
{
	char* s;
	if (size == 0) throw Empty_Stack();
	s = l.get();
	l.remove();
	size--;
	return s;
}

char* stack::peek () const throw (Stack_Err)
{
	if (size == 0) throw Empty_Stack();
	return l.get();
}

stack& stack::operator + (const char* s) throw (Stack_Err)
{
	if (size == max_size) throw Full_Stack();
	l.add(s);
	size++;
	return *this;
}

stack& stack::operator - (char*& s) throw (Stack_Err)
{
	if (size == 0) throw Empty_Stack();
	s = l.get();
	l.remove();
	size--;
	return *this;
}

stack& stack::operator = (const stack& st)
{
	l = st.l;
	size = st.size;
	return *this;
}


bool stack::operator == (const stack& st) const
{
	if (size != st.size) return 0;
	bool res = 1;
	node* p1 = l.head;
	node* p2 = st.l.head;
	while (res && p1)
	{
		if (strcmp(p1->str, p2->str) != 0) res = 0;
		p1 = p1->next;
		p2 = p2->next;
	}
	return res;
}

char* stack::operator [] (const int i) const throw (Stack_Err)
{
	char* s;
	if (i < 0 || i >= size) throw Wrong_Index();
	node* p = l.head;
	int k = 0;
	while (k < i)
	{
		p = p->next;
		k++;
	}
	s = new char[strlen(p->str)+1];
	strcpy(s,p->str);
	return s;
}

stack::operator char* () const throw (Stack_Err)
{
	char* s;
	if (l.head == NULL) throw Empty_Stack();

	/*node* p = l.head;
	int length = strlen(p->str)+1;
	char* s1 = new char[length];
	strcpy(s1,p->str);
	char* s2 = s1;
	while (p->next != NULL)
	{
		p = p->next;
		length += strlen(p->str);
		s1 = new char[length];
		strcpy(s1,s2);
		strcat(s1,p->str);
		delete [] s2;
		s2 = s1;
	}*/

	int length = 1;
	node* p = l.head;
	while (p != NULL)
	{
		length += strlen(p->str);
		p = p->next;
	}
	p = l.head;
	s = new char[length];
	strcpy(s,p->str);
	p = p->next;
	while (p != NULL)
	{
		strcat(s,p->str);
		p = p->next;
	}
	return s;
}
