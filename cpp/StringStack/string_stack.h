#include <cstring>

class Stack_Err
{
	public:
		virtual void print() = 0;
};

class Empty_Stack: public Stack_Err
{
	public:
		void print();
};

class Full_Stack: public Stack_Err
{
	public:
		void print();
};

class Wrong_Index: public Stack_Err
{
	public:
		void print();
};

struct node
{
	char* str;
	node* next;
	
	node ()
	{
		next = NULL;
    	}

   	 ~node ()
    	{
        	delete str;
    	}
};

struct list
{
	node* head;
 
	list ()
        {
       	    head = NULL;
 	}

	list (const list&);
       	void add (const char*);
        void remove ();
        char* get () const;
    	list& operator = (const list&);

        ~list ()
        {
		if (head != NULL)
		{
			node* p = head;
			while (p->next != NULL)
			{
				node* tmp = p;
				p = p->next;
				delete tmp;
			}
	            	delete p;
		}
        }
};

const int max_size = 256;

class stack
{
	list l;
	int size;
	public:
        stack ()
        {
        	l.head = NULL;
            	size = 0;
        }

	stack (const stack& st)
	{
		l = st.l;
		size = st.size;
	}

	void print () const throw (Stack_Err);
        void push (const char* s) throw (Stack_Err);
	char* pop () throw (Stack_Err);
	char* peek () const throw (Stack_Err);

        int length () const
        {
            	return size;
        }

        int maxsize () const
        {
            	return max_size;
        }

	bool isfull () const
	{
		if (size == max_size) return 1;
		return 0;
	}

	bool isempty () const
	{
		if (size == 0) return 1;
		return 0;
	}

        stack& operator + (const char* s) throw (Stack_Err);
        stack& operator - (char*& s) throw (Stack_Err);
        stack& operator = (const stack& st);
	bool operator == (const stack& st) const;
	bool operator != (const stack& st) const
	{
		return !(*this == st);
	}
	char* operator [] (const int i) const throw (Stack_Err);
	operator char* () const throw (Stack_Err);
};

/*class mystring
{
	char* str;
	int size;
	public:
		mystring ()
		{
			str = new char(0);
			size = 0;
		}

		mystring (const char* s)
		{
			str = new char[(size = strlen(s))+1];
			strcpy(str,s);
		}

		mystring (const char c)
		{
			str = new char[2];
			size = 1;
			str[0] = c;
			str[1] = '\0';
		}

		mystring (const mystring& s)
		{
			str = new char[(size = s.size)+1];
			strcpy(str,s.str);
		}

		~mystring ()
		{
			delete []str;
		}

		mystring& operator = (const char* s)
		{
			delete []str;
			str = new char[(size = strlen(s))+1];
			strcpy(str,s);
			return *this;
		}

		mystring& operator = (const mystring& s)
		{
			if (this == &s) return *this;
			delete []str;
			str = new char[(size = strlen(s.str))+1];
			strcpy(str,s.str);
			return *this;
		}

		mystring& operator = (const char c)
		{
			delete []str;
			str = new char [2];
			size = 1;
			str[0] = c;
			str[1] = '\0';
			return *this;
		}
};*/
