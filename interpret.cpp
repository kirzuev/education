#include <iostream>
#include <cstring>
#include <cstdio>

using namespace std;

enum lextype
{
	LEX_NULL,      //0
	
	LEX_PROGRAM,   //1
	LEX_START,     //2
	LEX_FINISH,    //3
	LEX_FIN,       //4 
	
	LEX_INT,       //5
	LEX_STRING,    //6
	LEX_BOOL,      //7
	LEX_REAL,      //8
	LEX_STRUCT,    //9

	LEX_ID,        //10
	LEX_NUM,       //11
	LEX_TRUE,      //12
	LEX_FALSE,     //13
	LEX_DOUBLE,    //14
	LEX_STR,       //15
	LEX_STRUCTURE, //16
	
	LEX_ELSE,      //17
	LEX_OF,        //18
	LEX_END,       //19

	LEX_IF,        //20
	LEX_CASE,      //21

	LEX_WHILE,     //22
	LEX_DO,        //23
	LEX_FOR,       //24
	
	LEX_READ,      //25
	LEX_WRITE,     //26

	LEX_GOTO,      //27
	LEX_CONTINUE,  //28
	LEX_BREAK,     //29
	
	LEX_NOT,       //30
	LEX_PLUS,      //31 +
	LEX_MINUS,     //32 -
	
	LEX_AND,       //33
	LEX_OR,        //34
	
	LEX_SEMICOLON, //35 ;
	LEX_COMMA,     //36 ,
	LEX_COLON,     //37 :
	LEX_ASSIGN,    //38 =
	
	LEX_LPAREN,    //39 (
	LEX_RPAREN,    //40 )
	LEX_LBRACE,    //41 {
	LEX_RBRACE,    //42 }
	
	LEX_EQ,        //43 ==
	LEX_LSS,       //44 <
	LEX_GTR,       //45 >
	LEX_LEQ,       //46 <=
	LEX_NEQ,       //47 !=
	LEX_GEQ,       //48 >=
	
	LEX_TIMES,     //49 *
	LEX_SLASH,     //50 /
	LEX_PERCENT,   //51 %

	POLIZ_LABEL,   //52
	POLIZ_ADDRESS, //53
	POLIZ_GO,      //54
	POLIZ_FGO      //55
};

class Error
{
protected:
	string _s;
public:
	virtual void print() = 0;
};	

class SemErr: public Error
{
	public:
		SemErr(string err)
		{
			_s = err;
		}
		virtual void print()
		{
			cerr << ' ' << _s << " ***" << endl << endl;
		}
};

class ExecErr: public Error
{
	public:
		ExecErr(string err)
		{
			_s = err;
		}
		virtual void print()
		{
			cerr << endl << "\t\t" << _s << endl << endl;
		}
};

class StErr: public Error
{
	public:
		StErr(string err)
		{
			_s = err;
		}
		virtual void print()
		{
			cerr << endl << "\t\t" << _s << endl << endl;
		}
};

template <class T, int maxsize>
class Stack
{
	T _s[maxsize];
	int _top;
public:
	Stack()
	{
		_top = 0;
	}
	void reset()
	{
		_top = 0;
	}
	bool isfull()
	{
		return _top == maxsize;
	}
	bool isempty()
	{
		return _top == 0;
	}
	void push(T t)
	{
		if ( !isfull() )
		{
			_s[_top] = t;
			_top++;
		}
		else
			throw StErr("Stack is full!");
	}
	T pop()
	{
		if( !isempty() )
		{
			_top--;
			return _s[_top];
		}
		else
			throw StErr("Stack is empty!");
	}
	T peek()
	{
		if( !isempty() )
		{
			return _s[_top-1];
		}
		else
			throw StErr("Stack is empty!");
	}
};

class Lex
{
	lextype _t_lex;
	int _v_lex;
public:
	Lex (lextype t  = LEX_NULL, int v = 0)
	{
		_t_lex = t;
		_v_lex = v;
	}
	Lex& operator = (const Lex& l)
	{
		_t_lex = l._t_lex;
		_v_lex = l._v_lex;
		return *this;
	}
	lextype gettype () const
	{
		return _t_lex;
	}
	int getvalue () const
	{
		return _v_lex;
	}
	friend ostream& operator << (ostream& s, const Lex& l)
	{
		s << '(' << l._t_lex << ',' << l._v_lex << ')';
		return s;
	}
};

class Ident
{
	const char* _name;
	const char* _struct_name;
	bool _declare;
	lextype _type;
	bool _assign;
	int _value;
public:
	Ident ()
	{
		_declare = false;
		_assign = false;
		_type = LEX_NULL;
		_value = 0;
		_name = 0;
		_struct_name = 0;
	}
	Ident& operator = (const Ident &p)
	{	
		_declare = p._declare;
		_type = p._type;
		_assign = p._assign;
		_value = p._value;
		delete [] _name;
		_name = 0;
		char* str = new char[strlen(p._name) + 1];
		strcpy(str, p._name);
		_name = str;
		return *this;
	}
	const char* get_structname () const
	{
		return _struct_name;	
	}
	void set_structname(const char*s)
	{
		char* str = new char[strlen(s) + 1];
		strcpy(str, s);
		delete [] _struct_name;
		_struct_name = str;
		str = 0;
	}
	const char* getname () const
	{
		return _name;	
	}
	void setname (const char* s)
	{
		char* str = new char[strlen(s) + 1];
		strcpy(str, s);
		delete [] _name;
		_name = str;
		str = 0;
	}
	bool getdeclare () const
	{
		return _declare;
	}
	void setdeclare ()
	{
		_declare = true;
	}
	lextype gettype () const
	{
		return _type;
	}
	void settype (lextype t)
	{
		_type = t;
	}
	bool getassign () const
	{
		return _assign;
	}
	void setassign ()
	{
		_assign = true;
	}
	int getvalue () const
	{
		return _value;
	}
	void setvalue (int v)
	{
		_value = v;
	}
};

class TablIdent
{
	int _size;
	int _top;
public:
	Ident* _p;
	TablIdent (int maxsize = 10)
	{
			_p = new Ident [_size = maxsize];
			_top = 0;
	}
	~TablIdent ()
	{
		delete [] _p;
	}
/*	void operator = (TablIdent& p)
	{
		int i = 0;
		delete [] _p;
		_p = new Ident[_size = p._size];
		_top = p._top;
		while (i <= _top)
		{
			_p[i] = p._p[i];
			i++;
		}
	}
*/	Ident& operator [] (int k)
	{
		return _p[k];
	}
	void print()
	{
		int i = 0;
		while (i < _top)
		{
//			cerr << _p[i].getname() << endl;
			i++;
		}
	}
	void clear_tabl (int maxsize = 10)
	{
		delete [] _p;
		_p = new Ident [_size = maxsize];
		_top = 0;
	}
	TablIdent& operator = (const TablIdent &t)
	{
		delete [] _p;
		_size = t._size;
		_p = new Ident[_size];
		_top = 0;
		while (_top < t._top){
			_p[_top] = (t._p)[_top];
			_top++;
		}
		return *this;
	}
	TablIdent& operator + (const TablIdent &t)
	{
		delete [] _p;
		_size = 2 * t._size;
		_p = new Ident[_size];
		_top = 0;
		while (_top < t._top){
			_p[_top] = (t._p)[_top];
			_top++;
		}
		return *this;
	}
	int put (const char* buf)
	{
		if (_top == _size)
		{
			TablIdent table;
			table + (*this);
			(*this) = table;
		}			
/*		int j;
		for (j = 1; j < _top; j++)
			if (strcmp(buf, _p[j].getname()) == 0)
				return j;
*/
		_p[_top].setname(buf);
		_top++;
		return _top-1;
	}
	int look (const char* buf)
	{
		int j;
		for (j = 0; j < _top; j++)
			if (strcmp(buf, _p[j].getname()) == 0)
				return j;
		return -1;
	}
};

struct selem
{
	const char* _nels;
	lextype _lels;
	selem* _nextels;
};

struct slist
{
	const char* _ns;
	selem* _els;
	slist* _nexts;
};

class TablStruct
{
	slist* _fs;
	slist* _ls;
public:
	TablStruct();
	~TablStruct();
	
	bool look_s(const char* name);
	void new_s(const char* name);
	bool look_els(const char* name);	
	void new_els (const char* name, lextype ltype);
	void print_TSct ()
	{
		slist* p = _fs;
		while (p != 0)
		{
//			cerr << p->_ns << endl;
			p = p->_nexts;
		}
	}
	
	selem* get_els(const char* name);
};

TablStruct TSct;

TablStruct::TablStruct()
{
	_fs = 0;
	_ls = 0;
}

TablStruct::~TablStruct()
{
	selem* p = 0;
	while (_fs != 0)
	{
		_ls = _fs->_nexts;
		_fs->_nexts = 0;
		while (_fs->_els != 0)
		{
			p = _fs->_els->_nextels;
			_fs->_els->_nextels = 0;
			delete [] _fs->_els->_nels;
			_fs->_els->_nels = 0;
			delete _fs->_els;
			_fs->_els = p;
		}
		delete [] _fs->_ns;
		_fs->_ns = 0;
		delete _fs;
		_fs = _ls;
	}
}

bool TablStruct::look_s(const char* name)
{
	slist* p = _fs;
	while (p != 0)
	{
		if (strcmp(p->_ns, name) == 0)
		{
			p = 0;
			return true;
		}
		p = p->_nexts;
	}
	return false;
}

void TablStruct::new_s(const char* name)
{
	char* str = 0;
	str = new char[strlen(name) + 1];
	strcpy(str, name);
	if (_fs == 0)
	{
		_fs = new slist;
		_ls = _fs;
	}
	else
	{
		_ls->_nexts = new slist;
		_ls = _ls->_nexts;
	}
	_ls->_ns = str;
	_ls->_els = 0;
	_ls->_nexts = 0;
	str = 0;
}

bool TablStruct::look_els(const char* name)
{
	selem* p = _ls->_els;
	while (p != 0)
	{
		if (strcmp(p->_nels, name) == 0)
		{
			p = 0;
			return true;
		}
		p = p->_nextels;
	}
	return false;
}

void TablStruct::new_els (const char* name, lextype ltype)
{
	char* str = 0;
	str = new char[strlen(name) + 1];
	strcpy(str, name);
	selem* p = _ls->_els;
	if (p == 0)
	{
		_fs->_els = new selem;
		p = _fs->_els;
	}
	else
	{
		while (p->_nextels != 0)
		{
			p = p->_nextels;
		}
		p->_nextels = new selem;
		p = p->_nextels;
	}
	p->_nels = str;
	p->_lels = ltype;
	p->_nextels = 0;
	str = 0;
	p = 0;
}

selem* TablStruct::get_els(const char* name)
{
	slist* p = _fs;
	while (p != 0)
	{
		if (strcmp(p->_ns, name) == 0)
		{
			return p->_els;
		}
		p = p->_nexts;
	}
	return 0;
}

struct TablTablIdent
{
	TablIdent* _idt;
	TablTablIdent* _nt;
	
//	void operator = (TablTablIdent)
//	{
	
//	}
};

Stack <lextype, 100> _st_lex;
Stack <const char*, 100> _st_name;

class ListTablIdent
{
	TablTablIdent* _ft;
	TablTablIdent* _lt;
	
	lextype _lex_type;
	const char* _struct_name;
public:
	ListTablIdent();
	~ListTablIdent();
	
	void cmp_three();
	void cmp_two();
	void cmp_bool();
	void cmp_int();
	void new_t();
	void del_t();
	bool look_alltables(const char* name)
	{
		TablTablIdent* p = _ft;
		while (p != 0)
		{
			if (p->_idt->look(name) >= 0)
			{
				p = 0;
				return true;
			}
			p = p->_nt;
		}
		return false;
	}

	lextype get_typeid(const char* name)
	{
		TablTablIdent* p = _lt, *q = 0;
		int i = 0;
		while (p != _ft)
		{
			if ((i = p->_idt->look(name)) >= 0)
			{
				return p->_idt->_p[i].gettype();
			}
			q = _ft;
			while (q->_nt != p)
			{
				q = q->_nt;
			}
			p = q;
			q = 0;
		}
		if (p != 0)
		{
			if ((i = p->_idt->look(name)) >= 0)
			{
				return p->_idt->_p[i].gettype();
			}
			p = 0;
		}
		return LEX_NULL;
	}
	
	const char* get_idstructname(const char* name)
	{
		TablTablIdent* p = _lt, *q = 0;
		int i = 0;
		while (p != _ft)
		{
			if ((i = p->_idt->look(name)) >= 0)
			{
				return p->_idt->_p[i].get_structname();
			}
			q = _ft;
			while (q->_nt != p)
			{
				q = q->_nt;
			}
			p = q;
			q = 0;
		}
		if (p != 0)
		{
			if ((i = p->_idt->look(name)) >= 0)
			{
				return p->_idt->_p[i].get_structname();
			}
			p = 0;
		}
		return 0;
	}

	int get_value(const char* name)
	{
		TablTablIdent* p = _lt, *q = 0;
		int i = 0;
		while (p != _ft)
		{
			if ((i = p->_idt->look(name)) >= 0)
			{
				return p->_idt->_p[i].getvalue();
			}
			q = _ft;
			while (q->_nt != p)
			{
				q = q->_nt;
			}
			p = q;
			q = 0;
		}
		if (p != 0)
		{
			if ((i = p->_idt->look(name)) >= 0)
			{
				return p->_idt->_p[i].getvalue();
			}
			p = 0;
		}
		return 0;
	}

	bool get_assign(const char* name)
	{
		TablTablIdent* p = _lt, *q = 0;
		int i = 0;
		while (p != _ft)
		{
			if ((i = p->_idt->look(name)) >= 0)
			{
				return p->_idt->_p[i].getassign();
			}
			q = _ft;
			while (q->_nt != p)
			{
				q = q->_nt;
			}
			p = q;
			q = 0;
		}
		if (p != 0)
		{
			if ((i = p->_idt->look(name)) >= 0)
			{
				return p->_idt->_p[i].getassign();
			}
			p = 0;
		}
		return false;
	}

	void put_assign(const char* name)
	{
		TablTablIdent* p = _lt, *q = 0;
		int i = 0;
		while (p != _ft)
		{
			if ((i = p->_idt->look(name)) >= 0)
			{
				p->_idt->_p[i].setassign();
			}
			q = _ft;
			while (q->_nt != p)
			{
				q = q->_nt;
			}
			p = q;
			q = 0;
		}
		if (p != 0)
		{
			if ((i = p->_idt->look(name)) >= 0)
			{
				p->_idt->_p[i].setassign();
			}
			p = 0;
		}
	}

	void put_value(const char* name, int v)
	{
		TablTablIdent* p = _lt, *q = 0;
		int i = 0;
		while (p != _ft)
		{
			if ((i = p->_idt->look(name)) >= 0)
			{
				p->_idt->_p[i].setvalue(v);
			}
			q = _ft;
			while (q->_nt != p)
			{
				q = q->_nt;
			}
			p = q;
			q = 0;
		}
		if (p != 0)
		{
			if ((i = p->_idt->look(name)) >= 0)
			{
				p->_idt->_p[i].setvalue(v);
			}
			p = 0;
		}
	}

	bool look_id(const char* name);
	void new_id(const char* name);

	lextype get_lextype() { return _lex_type;}
	const char * get_structname() { return _struct_name;}
	void set_lextype (lextype ltype) { _lex_type = ltype;}
	void set_structname (const char* str)
	{
		char* s = new char[strlen(str) + 1];
		strcpy(s, str);
		delete [] _struct_name;
		_struct_name = s;
		s = 0;
	}
	
};

ListTablIdent LTID;

ListTablIdent::ListTablIdent()
{
	_ft = 0;
	_lt = 0;
	_lex_type = LEX_NULL;
	_struct_name = 0;
}

ListTablIdent::~ListTablIdent()
{
	delete [] _struct_name;
	_struct_name = 0;
	if (_ft != 0)
	{
		delete _ft->_idt;
		_ft->_idt = 0;
		delete _ft;
	}
	_ft = 0;
	_lt = 0;
}

void ListTablIdent::cmp_three()
{
	lextype ax = LEX_NULL, bx = LEX_NULL, cx = LEX_NULL; 
	ax = _st_lex.pop();
	bx = _st_lex.pop();
	cx = _st_lex.pop();
	
	if (ax == LEX_NUM) { ax = LEX_INT; }
	if (ax == LEX_TRUE || ax == LEX_FALSE) { ax = LEX_BOOL; }
	if (ax == LEX_DOUBLE) { ax = LEX_REAL; }
	if (ax == LEX_STR) { ax = LEX_STRING; }
	
	if (cx == LEX_NUM) { cx = LEX_INT; }
	if (cx == LEX_TRUE || cx == LEX_FALSE) { cx = LEX_BOOL; }
	if (cx == LEX_DOUBLE) { cx = LEX_REAL; }
	if (cx == LEX_STR) { cx = LEX_STRING; }
	switch (bx)
	{
		case LEX_ASSIGN:
			if ((ax == cx) || ((ax == LEX_INT) && (cx == LEX_REAL)) || ((ax == LEX_REAL) && (cx == LEX_INT)))
			{
				if (ax == LEX_STRUCT)
				{
					const char * s1 = _st_name.pop();
					const char * s2 = _st_name.pop();
					if (strcmp(s1, s2) != 0) { throw SemErr("different struct names '='"); }
					_st_name.push(s1);
					s1 = 0;
					s2 = 0;	
				}
				_st_lex.push(ax);
				break;
			}
			throw SemErr("unallowed types '='");
		case LEX_EQ:
		case LEX_LSS:
		case LEX_GTR:
		case LEX_NEQ: 
			if ((ax == LEX_INT || ax == LEX_REAL) && (cx == LEX_INT || cx == LEX_REAL) || ax == cx && ax == LEX_STRING)
			{
				_st_lex.push(LEX_BOOL);
				break;
			}
			throw SemErr("unallowed types in comparison");
		case LEX_LEQ:
		case LEX_GEQ:
			if ((ax == LEX_INT || ax == LEX_REAL) && (cx == LEX_INT || cx == LEX_REAL))
			{
				_st_lex.push(LEX_BOOL);
				break;
			}
			throw SemErr("unallowed types in comparison");
		case LEX_PLUS:
			if (ax == LEX_INT && cx == LEX_INT || ax == LEX_REAL && cx == LEX_REAL || ax == LEX_STRING ||cx == LEX_STRING)
			{	
				_st_lex.push(ax);
				break;
			}
			if (ax == LEX_INT && cx == LEX_REAL || ax == LEX_REAL && cx == LEX_INT)
			{
				_st_lex.push(LEX_REAL);
				break;
			}
			throw SemErr("unallowed types");
		case LEX_PERCENT:
			if (ax == LEX_INT && cx == LEX_INT)
			{
				_st_lex.push(ax);
				break;
			}
			throw SemErr("unallowed types");
		case LEX_MINUS:
		case LEX_TIMES:
		case LEX_SLASH:
			if ((ax == LEX_INT || ax == LEX_REAL) && (cx == LEX_INT || cx == LEX_REAL))
			{
				if (ax == LEX_INT && cx == LEX_INT)
				{
					_st_lex.push(ax);
					break;
				}
				_st_lex.push(LEX_REAL);
				break;
			}
			throw  SemErr("unallowed types");
		case LEX_AND:
		case LEX_OR:
			if (ax == LEX_BOOL && cx == LEX_BOOL)
			{
				_st_lex.push(ax);
				break;
			}
			throw SemErr("unallowed types");
		default:
			throw SemErr("unallowed operator");
	}
}

void ListTablIdent::cmp_two()
{
	lextype ax = LEX_NULL, bx = LEX_NULL; 
	bx = _st_lex.pop();
	ax = _st_lex.pop();
	
	if (bx == LEX_NUM) { bx = LEX_INT; }
	if (bx == LEX_TRUE || bx == LEX_FALSE) { bx = LEX_BOOL; }
	if (bx == LEX_DOUBLE) { bx = LEX_REAL; }
	
	if (ax == LEX_PLUS && bx == LEX_PLUS || ax == LEX_MINUS && bx == LEX_MINUS)
	{
		_st_lex.push(LEX_PLUS);
		return;
	}
	if (ax == LEX_PLUS && bx == LEX_MINUS || ax == LEX_MINUS && bx == LEX_PLUS)
	{
		_st_lex.push(LEX_MINUS);
		return;
	}
	if (ax == LEX_NOT && bx == LEX_NOT)
	{
		_st_lex.push(LEX_TIMES);
		return;
	}
	if (ax == LEX_TIMES && bx == LEX_NOT)
	{
		_st_lex.push(LEX_NOT);
		return;
	}
	switch(ax)
	{
		case LEX_PLUS:
		case LEX_MINUS:
			if (bx == LEX_INT || bx == LEX_REAL)
			{
				_st_lex.push(bx);
				break;
			}
			throw SemErr("unallowed types");
		case LEX_NOT:
		case LEX_TIMES:
			if (bx == LEX_BOOL)
			{
				_st_lex.push(bx);
				break;
			}
			throw SemErr("unallowed types");
	}
}

void ListTablIdent::cmp_bool()
{
	lextype ax = LEX_NULL;
	ax = _st_lex.pop();
	if (ax != LEX_BOOL && ax != LEX_TRUE && ax != LEX_FALSE){ throw SemErr("not bool expression");}
	_st_lex.push(ax);
}

void ListTablIdent::cmp_int()
{
	lextype ax = LEX_NULL;
	ax = _st_lex.pop();
	//if (ax != LEX_INT){ throw SemErr("not bool expression");}
	if (ax != LEX_INT && ax != LEX_NUM){ throw SemErr("not int expression");}
	_st_lex.push(ax);
}

void ListTablIdent::new_t()
{
	if (_ft == 0)
	{
		_ft = new TablTablIdent;
		_lt = _ft;	
	}
	else
	{
		_lt->_nt = new TablTablIdent;
		_lt = _lt->_nt;
	}
	_lex_type = LEX_NULL;
	delete [] _struct_name;
	_struct_name = 0;
	_lt->_idt = new TablIdent;
	_lt->_nt = 0;
}

void ListTablIdent::del_t()
{
	TablTablIdent* p = _lt;
	_lt = _ft;
	while (_lt->_nt != p)
	{
		_lt = _lt->_nt;
	}
	_lt->_nt = 0;
	delete p->_idt;
	p->_idt = 0;
	delete p;
	p = 0;
}

bool ListTablIdent::look_id(const char* name)
{
	if (_lt->_idt->look(name) >= 0)
	{
		return true;
	}
	return false;
}

void ListTablIdent::new_id(const char* name)
{
	int i = _lt->_idt->put(name);
	int j = 0;
	_lt->_idt->_p[i].settype(_lex_type);
	_lt->_idt->_p[i].setdeclare();
	if (_lex_type == LEX_STRUCT)
	{
		_lt->_idt->_p[i].set_structname(_struct_name);
		selem* p;
		p = TSct.get_els(_struct_name);
		char* str = 0;
		j = strlen(name);
		while (p != 0)
		{
			str = new char[j + strlen(p->_nels) + 2];
			strcpy(str, name);
			str[j] = '.';
			strcpy(&str[j+1], p->_nels);
			i = _lt->_idt->put(str); 
			_lt->_idt->_p[i].settype(p->_lels);
			_lt->_idt->_p[i].setdeclare();
			str = 0;
			p = p->_nextels;
		}
	}
}

class TablReal
{
	double* _p;
	int _size;
	int _top;
public:
	TablReal (int maxsize = 10)
	{
		_p = new double [_size = maxsize];
		_top = 0;
	}
	~TablReal ()
	{
		delete [] _p;
	}
	double& operator [] (int k)
	{
		return _p[k];
	}
	TablReal& operator = (const TablReal &t)
	{
		delete [] _p;
		_size = t._size;
		_p = new double[_size];
		_top = 0;
		while (_top < t._top)
		{
			_p[_top] = (t._p)[_top];
			_top++;
		}
		return *this;
	}
	TablReal& operator + (const TablReal &t)
	{
		delete [] _p;
		_size = 2 * t._size;
		_p = new double[_size];
		_top = 0;
		while (_top < t._top)
		{
			_p[_top] = (t._p)[_top];
			_top++;
		}
		return *this;
	}
	int put (double x)
	{
		if (_top == _size)
		{
			TablReal table;
			table + (*this);
			(*this) = table;
		}			
		_p[_top] = x;
		_top++;
		return _top-1;
	} 
};

class TablStr
{
	char** _p;
	int _size;
	int _top;
public:
	TablStr (int maxsize = 10)
	{
		_p = new char* [_size = maxsize];
		_top = 0;
	}
	~TablStr ()
	{
		while (_top > 0)
		{
			_top--;
			delete [] _p[_top];
		}
		delete [] _p;
	}
	char*& operator [] (int k)
	{
		return _p[k];
	}
	TablStr& operator = (const TablStr &t)
	{
		_top = t._top;
		while (_top > 0)
		{
			_top--;
			delete [] _p[_top];
		}
		delete [] _p;
		_size = t._size;
		_p = new char*[_size];
		while (_top < t._top)
		{
			_p[_top] = new char[strlen(t._p[_top]) + 1];
			strcpy(_p[_top], t._p[_top]);
			_top++;
		}
		return *this;
	}
	TablStr& operator + (const TablStr &t)
	{
		while (_top > 0)
		{
			_top--;
			delete [] _p[_top];
		}
		delete [] _p;
		_size = 2 * t._size;
		_p = new char*[_size];
		while (_top < t._top)
		{
			_p[_top] = new char[strlen(t._p[_top]) + 1];
			strcpy(_p[_top], t._p[_top]);
			_top++;
		}
		return *this;
	}
	int put (char* s)
	{
		if (_top == _size)
		{
			TablStr table;
			table + (*this);
			(*this) = table;
		}			
		_p[_top] = new char [strlen(s)+1];
		strcpy(_p[_top],s);
		_top++;
		return _top-1;
	}
};

//TablTablIdent TTID(100);
TablIdent TID(10);
TablReal TR(10);
TablStr TS(10);

class Scanner
{
	enum state { H, LET, LET_DOT, LET_STRUCT, ZERO, NUMB, NUMB_DOT, NUMB_REAL, STR, COMM, OPER, NEQ, DELIM, LAST };
	state _CS;
	FILE* _fp;
	signed char _c;
	char _buf[64];
	int _buftop;
	int strnum;
	void clear ()
	{
//		int j;
		_buftop = 0;
//		for (j = 0; j < 64; j++)
//			_buf[j] = '\0';
	}
	void add (signed char c)
	{
		_buf[_buftop++] = c;
	}
	int look (const char* buf, const char** list) const
	{
		int i = 0;
		while (list[i] != NULL)
		{
			if (strcmp(buf, list[i]) == 0)
				return i;
			i++;
		}
		return 0;
	}
	void gc ()
	{
		_c = fgetc(_fp);
	}
	void ungc ()
	{
		ungetc(_c, _fp);
	}
	bool ischaracter();
public:
	static const char* _TW[];
	static lextype _words[];
	static const char* _TD[];
	static lextype _dlms[];
	const char* backlook (Lex l) const
	{
		int i = 1;
		lextype type = l.gettype();
		if (type == LEX_ID)
			return TID[l.getvalue()].getname();
		if (type == LEX_STR)
			return TS[l.getvalue()];
		
		while (_words[i] != LEX_NULL)
		{
			if (_words[i] == type)
				return _TW[i];
			i++;
		}
		i = 1;
		while (_dlms[i] != LEX_NULL)
		{
			if (_dlms[i] == type)
				return _TD[i];
			i++;
		}
		return "";
	}
	const int geterrstr() const
	{
		return strnum;
	}
	Scanner (const char* program)
	{
		strnum = 1;
		if ((_fp = fopen(program, "r")) == NULL) throw program;
		_CS = H;
		clear();
	}
	Lex getlex ();
};

const char* Scanner::_TW[] =
{
	"",        //0 - not used
	"and",     //1
	"bool",    //2
	"break",   //3
	"case",    //4
	"continue",//5
	"do",      //6
	"else",    //7
	"end",     //8
	"false",   //9
	"finish",  //10
	"for",     //11
	"goto",    //12
	"if",      //13
	"int",     //14
	"not",     //15
	"of",      //16
	"or",      //17
	"program", //18
	"read",    //19
	"real",    //20
	"start",   //21
	"string",  //22
	"struct",  //23
	"true",    //24
	"while",   //25
	"write",   //26
	NULL
};

const char* Scanner::_TD[] =
{
	"",   //0 - not used
	";",  //1
	",",  //2
	":",  //3
	"=",  //4
	"(",  //5
	")",  //6
	"{",  //7
	"}",  //8
	"==", //9
	"<",  //10
	">",  //11
	"+",  //12
	"-",  //13
	"*",  //14
	"/",  //15
	"<=", //16
	"!=", //17
	">=", //18
	"%",  //19
	"$",  //20
	NULL
};

lextype Scanner::_words[] =
{
	LEX_NULL,
	LEX_AND,
	LEX_BOOL,
	LEX_BREAK,
	LEX_CASE,
	LEX_CONTINUE,
	LEX_DO,
	LEX_ELSE,
	LEX_END,
	LEX_FALSE,
	LEX_FINISH,
	LEX_FOR,
	LEX_GOTO,
	LEX_IF,
	LEX_INT,
	LEX_NOT,
	LEX_OF,
	LEX_OR,
	LEX_PROGRAM,
	LEX_READ,
	LEX_REAL,
	LEX_START,
	LEX_STRING,
	LEX_STRUCT,
	LEX_TRUE,
	LEX_WHILE,
	LEX_WRITE,
	LEX_NULL
};

lextype Scanner::_dlms[] =
{
	LEX_NULL,
	LEX_SEMICOLON,
	LEX_COMMA,
	LEX_COLON,
	LEX_ASSIGN,
	LEX_LPAREN,
	LEX_RPAREN,
	LEX_LBRACE,
	LEX_RBRACE,
	LEX_EQ,
	LEX_LSS,
	LEX_GTR,
	LEX_PLUS,
	LEX_MINUS,
	LEX_TIMES,
	LEX_SLASH,
	LEX_LEQ,
	LEX_NEQ,
	LEX_GEQ,
	LEX_PERCENT,
	LEX_FIN,
	LEX_NULL
};

bool Scanner::ischaracter()
{
	if (_c == '+' || _c == '-' || _c == '*' || _c == '/' 
		|| _c == '%' || _c == ',' || _c == ';' || _c == '\'' 
		|| _c == '.' || _c == '(' || _c == ')' || _c == '{' 
		|| _c == '}' || _c == ':' || _c == ' ' || _c == '!'
		|| _c == '\t' || _c == '\n' || _c == '<' || _c == '>' || _c == '_')
	{
		if (_c == '\n') strnum++;
		return true;
	}
	return false;
}

Lex Scanner::getlex()
{
	int d = 0, j = 0;
	double r = 0;
	double x = 0;
	_CS = H;
	gc();
	do {
		switch (_CS)
		{
			case H:
				if (_c == ' ' || _c == '\n' || _c == '\t')
				{
					if (_c == '\n') strnum++;
					gc();
					break;
				}
				if (isalpha(_c) || _c == '_')
				{
					clear();
					add(_c);
					gc();
					_CS = LET;
					break;
				}
				if (_c == '0')
				{
					d = 0;
					gc();
					_CS = ZERO;
					break;
				}
				if (isdigit(_c))
				{
					d = _c-'0';
					gc();
					_CS = NUMB;
					break;
				}
				if (_c == '"')
				{
					clear();
					gc();
					_CS = STR;
					break;
				}
				if (_c == '#')
				{
					gc();
					_CS = COMM;
					break;
				}
				if (_c == '=' || _c == '<' || _c == '>')
				{
					clear();
					add(_c);
					gc();
					_CS = OPER;
					break;
				}
				if (_c == '!')
				{
					clear();
					add(_c);
					gc();
					_CS = NEQ;
					break;
				}
				if (_c == '+' || _c == '-' || _c == '*' || _c == '/' 
					|| _c == ',' || _c == ';' || _c == '(' || _c == ')' 
					|| _c == '{' || _c == '}' || _c == '%' || _c == ':')
				{
					clear();
					add(_c);
					add('\0');
					gc();
					_CS = DELIM;
					break;
				}
				if (_c == '$')
				{
					_CS = LAST;
					break;
					//return Lex(LEX_FIN);
				}
				throw _c;
				break;
			case LET:
				while (isalpha(_c) || isdigit(_c) || _c == '_')
				{
					add(_c);
					gc();
				}
				if (_c == '.')
				{
					add(_c);
//					add('\0');
					gc();
					_CS = LET_DOT;
					break;	
				}
				add('\0');
				if (j = look(_buf,_TW))
				{
					ungc();
					return Lex(_words[j], j);
				} 
				else 
				{
					j = TID.put(_buf);
					ungc();
					return Lex(LEX_ID, j);
				}
				break;
			case LET_DOT:
				if (isalpha(_c) || _c == '_'){
					add(_c);
					gc();
					_CS = LET_STRUCT;
					break;
				}
				throw _c;
				break;
			case LET_STRUCT:
				while (isalpha(_c) || isdigit(_c) || _c == '_')
				{
					add(_c);
					gc();
				}
				add('\0');
				j = TID.put(_buf);
				ungc();
				return Lex(LEX_STRUCTURE, j);
				break;
			case ZERO:
				if (_c == '.')
				{
					gc();
					_CS = NUMB_DOT;
					break;
				}
				if (ischaracter() || _c == '"' || _c == '#')
				{
					ungc();
					return Lex(LEX_NUM, d);
				}
				throw _c;
				break;
			case NUMB:
				while (isdigit(_c))
				{
					d = d*10 + (_c-'0');
					gc();
				}
				if (_c == '.')
				{
					gc();
					_CS = NUMB_DOT;
					break;
				}
				if (isalpha(_c))
				{
					throw _c;
				}
				ungc();
				return Lex(LEX_NUM, d);
				break;
			case NUMB_DOT:
				if (isdigit(_c))
				{
					r = 10;
					x = d + (_c-'0')/r;
					r *= 10;
					gc();
					_CS = NUMB_REAL;
					break;
				}
				throw _c;
				break;
			case NUMB_REAL:
				while (isdigit(_c))
				{
					x = x + (_c-'0')/r;
					r *= 10;
					gc();
				}
				add('\0');
				if (isalpha(_c))
				{
					throw _c;
				}
				j = TR.put(x);
				ungc();
				return Lex(LEX_DOUBLE, j);
				break;
			case STR:
				while (ischaracter() || isalpha(_c) || isdigit(_c) || _c == '#' || _c == '='|| _c == '\'' || _c == '\\' || _c == '?')
				{
					add(_c);
						gc();
				}
				if (_c == '"')
				{
					gc();
				}
				else
				{
					throw _c;
				}
				add('\0');
				j = TS.put(_buf);
				ungc();
				return Lex(LEX_STR,j);
				break;
			case COMM:
				while (ischaracter() || isalpha(_c) || isdigit(_c) || _c == '"' || _c == '\'' || _c == '\\' || _c == '?' || _c == '=')
				{
					gc();
				}
				if (_c == '#')
				{
					gc();
					_CS = H;
					break;
				}
				throw _c;
				break;
			case OPER:
				if (_c == '=')
				{
					add(_c);
					gc();
				}
				add('\0');
				j = look(_buf,_TD);
				ungc();
				return Lex(_dlms[j],j);
				break;
			case NEQ:
				if (_c == '=')
				{
					add(_c);
					gc();
					add('\0');
					j = look(_buf,_TD);
					ungc();
					return Lex(LEX_NEQ,j);
				}
				throw _c;
				break;
			case DELIM:
				if (j = look(_buf,_TD))
				{
					ungc();
					return Lex(_dlms[j],j);
				
				}
				break;
			case LAST:
				return Lex(LEX_FIN,look("$",_TD));
		}
	} while (true);
}

class Poliz
{
	Lex* _p;
	int _size;
	int _free;
public:
	Poliz(int maxsize)
	{
		_p = new Lex [_size = maxsize];
		_free = 0;
	}
	~Poliz()
	{
		delete []_p;
	}
	void putlex(Lex l)
	{
		_p[_free] = l;
		_free++;
	}
	void putlex(Lex l, int place)
	{
		_p[place] = l;
	}
	void blank()
	{
		_free++;
	}
	int getfree()
	{
		return _free;
	}
	Lex& operator [] (int index)
	{
		if (index > _size)
			throw ExecErr("*** POLIZ ERROR: out of array ***");
		else
			if (index > _free)
				throw ExecErr("*** POLIZ ERROR: indefinite element of array ***");
			else
				return _p[index];
	}
	void print()
	{
		int i;
		cerr << "POLIZ:" << endl;
		cerr << "================" << endl;
		for (i = 0; i < _free; i++)
		{
			cerr << i << ": ";
			lextype type = _p[i].gettype();
			if (type == LEX_ID || type == POLIZ_ADDRESS)
			{
				cerr << TID[_p[i].getvalue()].getname() << endl;
				continue;
			}
			if (type == LEX_STR)
			{
				cerr << TS[_p[i].getvalue()] << endl;
				continue;
			}
			if (type == LEX_DOUBLE)
			{
				cerr << TR[_p[i].getvalue()] << endl;
				continue;
			}
			if (type == LEX_NUM)
			{
				cerr << _p[i].getvalue() << endl;
				continue;
			}
			if (type == POLIZ_LABEL)
			{
				cerr << _p[i].getvalue() << endl;
				continue;
			}
			if (type == POLIZ_FGO)
			{
				cerr << "!F" << endl;
				continue;
			}
			if (type == POLIZ_GO)
			{
				cerr << "!" << endl;
				continue;
			}
			int j = 1; bool printed = false;
			while (Scanner::_words[j] != LEX_NULL)
			{
				if (Scanner::_words[j] == type)
				{
					printed = true;
					cerr << Scanner::_TW[j] << endl;
				}
				j++;
			}
			if (printed) continue;
			j = 1;
			while (Scanner::_dlms[j] != LEX_NULL)
			{
				if (Scanner::_dlms[j] == type)
				{
					printed = true;
					cerr << Scanner::_TD[j] << endl;
				}
				j++;
			}
			if (printed) continue;
			cerr << _p[i] << endl;
		}
		cerr << "================" << endl;
	}
};

struct arg
{
	union value
	{
		int i;
		double r;
		char* s;
	} _v;
	int _i;
	lextype _t;
	arg()
	{
		_i = 0;
		_t = LEX_NULL;
	}
	arg(int val, lextype type)
	{
		_t = type;
		switch (_t)
		{
			case LEX_DOUBLE:
				_v.r = TR[val];
				_i = val;
				break;
			case LEX_STR:
				_v.s = new char [strlen(TS[val]) + 1];
				strcpy(_v.s,TS[val]);
				_i = val;
				break;
			case LEX_STRUCT:
				_i = val;
				break;
			default:
				_v.i = val;
		}
	}
	friend ostream& operator << (ostream& s, const arg& a)
	{
		switch (a._t)
		{
			case LEX_TRUE:
			case LEX_FALSE:
			case LEX_NUM:
				cerr << a._v.i;
				break;
			case LEX_DOUBLE:
				cerr << a._v.r;
				break;
			case LEX_STR:
				cerr << a._v.s;
				break;
			default:
				cerr << a._v.i;
				break;
				throw ExecErr("*** EXECUTION ERROR: wrong type of operand on <WRITE> ***");
		}
		return s;
	}
	arg(const arg& a)
	{
		switch (a._t)
		{
/*			case LEX_TRUE:
			case LEX_FALSE:
			case LEX_NUM:
				_v.i = a._v.i;
				_t = a._t;
				break;
*/			case LEX_DOUBLE:
				_v.r = a._v.r;
				_t = LEX_DOUBLE;
				_i = a._i;
				break;
			case LEX_STR:
				_v.s = new char [strlen(a._v.s) + 1];
				strcpy(_v.s,a._v.s);
				_t = LEX_STR;
				_i = a._i;
				break;
			default:
				_v.i = a._v.i;
				_t = a._t;
//				throw ExecErr("*** EXECUTION ERROR: wrong type of operand on copy constructor ***");
		}
	}
	arg& operator = (arg a)
	{
		switch (a._t)
		{
/*			case LEX_TRUE:
			case LEX_FALSE:
			case LEX_NUM:
				_v.i = a._v.i;
				_t = a._t;
				break;
*/			case LEX_DOUBLE:
				_v.r = a._v.r;
				_t = LEX_DOUBLE;
				_i = a._i;
				break;
			case LEX_STR:
				_v.s = new char [strlen(a._v.s) + 1];
				strcpy(_v.s,a._v.s);
				_t = LEX_STR;
				_i = a._i;
				break;
			default:
				_v.i = a._v.i;
				_t = a._t;
//				throw ExecErr("*** EXECUTION ERROR: wrong type of operand on operator =  ***");
		}
		return *this;
	}
	arg& operator + (arg a)
	{
		switch (_t)
		{
			case LEX_NUM:
				if (a._t == LEX_NUM)
					_v.i += a._v.i;
				else
					_v.i += a._v.r;
				break;
			case LEX_DOUBLE:
				if (a._t == LEX_DOUBLE)
					_v.r += a._v.r;
				else 
					_v.r += a._v.i;
				TR[_i] = _v.r;
				break;
			case LEX_STR:
			{
				char *s = new char [ strlen(TS[_i]) + strlen(TS[a._i]) + 1 ];
				strcpy(s,TS[_i]);
				strcat(s,TS[a._i]);
				TS[_i] = s;
				delete _v.s;
				_v.s = new char [ strlen(TS[_i]) + 1 ];
				strcpy(_v.s,TS[_i]);
			}
				break;
			default:
				throw ExecErr("*** EXECUTION ERROR: wrong type of operand on operator + ***");
		}
		return *this;
	}
	arg& operator * (arg a)
	{
		switch (_t)
		{
			case LEX_NUM:
				if (a._t == LEX_NUM)
					_v.i *= a._v.i;
				else
					_v.i *= a._v.r;
				break;
			case LEX_DOUBLE:
				if (a._t == LEX_DOUBLE)
					_v.r *= a._v.r;
				else
					_v.r *= a._v.i;
				TR[_i] = _v.r;
				break;
			default:
				throw ExecErr("*** EXECUTION ERROR: wrong type of operand on operator * ***");
		}
		return *this;
	}
	arg& operator - (arg a)
	{
		switch (_t)
		{
			case LEX_NUM:
				if (a._t == LEX_NUM)
					_v.i -= a._v.i;
				else
					_v.i -= a._v.r;
				break;
			case LEX_DOUBLE:
				if (a._t == LEX_DOUBLE)
					_v.r -= a._v.r;
				else
					_v.r -= a._v.i;
				TR[_i] = _v.r;
				break;
			default:
				throw ExecErr("*** EXECUTION ERROR: wrong type of operand on operator - ***");
		}
		return *this;
	}
	arg& operator / (arg a)
	{
		switch (_t)
		{
			case LEX_NUM:
				if (a._v.i != 0)
					if (a._t == LEX_NUM)
						_v.i /= a._v.i;
					else
						_v.i /= a._v.r;
				else
					throw ExecErr("*** EXECUTION ERROR: divide by zero ***");
				break;
			case LEX_DOUBLE:
				if (a._v.r != 0)
				{
					if (a._t == LEX_DOUBLE)
						_v.r /= a._v.r;
					else
						_v.r /= a._v.i;
					TR[_i] = _v.r;
				}
				else
					throw ExecErr("*** EXECUTION ERROR: divide by zero ***");
				break;
			default:
				throw ExecErr("*** EXECUTION ERROR: wrong type of operand on operator / ***");
		}
		return *this;
	}
	arg& operator % (arg a)
	{
		if (_t != LEX_NUM || a._t != LEX_NUM)
			throw ExecErr("*** EXECUTION ERROR: wrong type of operand on operator % ***");
		if (a._v.i != 0)
			_v.i %= a._v.i;
		else
			throw ExecErr("*** EXECUTION ERROR: divide by zero ***");
		return *this;
	}
};

struct _ends
{
	int label;
	_ends* next;
};

struct Tends
{
	_ends* p;
};

class Parser
{
	Lex _p_lex;
	Lex _n_lex;
	Lex _c_lex;
	lextype _p_type;
	lextype _n_type;
	lextype _c_type;
	int _p_val;
	int _n_val;
	int _c_val;
	Scanner _scan;
	Stack <Lex, 100> _st_ops; 
	Stack <int, 100> _st_labels; 
	Stack <Tends, 100> _st_ends; 

	void program();
	void strct();
	void body();
	void definition();
	void type();
	void variable();
	void instruction(); // оператор
	void stipulation(); // условие
	void cycle();
	void input_output();
	void jump();
	void tagged_instr();
	void expression();
	void action();
	void bool_sum();
	void bool_mul();
	void cmp_variable();
	void sum();
	void mul_with_sign();
	void mul();

	void quant_expr();
	void variants();
	void variant();
	void cmp_instr();
	void plus_minus();
	void mul_div_exc(); //excess избыток, остаток... 
	void un_not();

	void gl()
	{
		if (_n_type == LEX_NULL)
		{
			_p_lex = _c_lex;
			_p_type = _c_type;
			_p_val = _c_val;
			_c_lex = _scan.getlex();
			_c_type = _c_lex.gettype();
			_c_val = _c_lex.getvalue();
		}
		else
		{
			_p_lex = _c_lex;
			_p_type = _c_type;
			_p_val = _c_val;
			_c_lex = _n_lex;
			_c_type = _n_type;
			_c_val = _n_val;
			_n_lex = Lex(LEX_NULL,0);
			_n_type = LEX_NULL;
			_n_val = 0;
		}
			
	}
	void ungl()
	{
		_n_lex = _c_lex;
		_n_type = _c_type;
		_n_val = _c_val;
		_c_lex = _p_lex;
		_c_type = _p_type;
		_c_val = _p_val;
	}
public:
	Poliz _prog;
	Parser (const char* prog): _scan(prog), _prog(1000)
	{
		_n_lex = Lex(LEX_NULL,0);
		_n_type = LEX_NULL;
		_n_val = 0;
		_c_lex = Lex(LEX_NULL,0);
		_c_type = LEX_NULL;
		_c_val = 0;
		_p_lex = Lex(LEX_NULL,0);
		_p_type = LEX_NULL;
		_p_val = 0;
	}
	const char* lexstr(Lex l) const
	{
		return _scan.backlook(l);
	}
	const int errstr() const
	{
		return _scan.geterrstr();
	}
	void analyze();
		
};

void Parser::analyze()
{
	cout << "_______analyze() : begin" << endl;
	gl();
	program();
	_prog.print();
	cout << "_______analyze() : end" << endl;
}

void Parser::program()
{
	cout << "_______program() : begin" << endl;
	if (_c_type != LEX_PROGRAM) { throw _c_lex;}
	gl();
	LTID.new_t();
	while (_c_type == LEX_STRUCT)
	{ 
		strct();
		if (_c_type != LEX_SEMICOLON) { throw _c_lex;}
		gl();
	}
	body();
	if (_c_type != LEX_FIN) { throw _c_lex;}
	cout << "_______program() : end" << endl;
}

void Parser::strct()
{
	cout << "_______strct() : begin" << endl;
	if (_c_type != LEX_STRUCT) { throw _c_lex;}
	gl();
	if (_c_type != LEX_ID) { throw _c_lex;}
	LTID.set_structname(TID[_c_val].getname());
	if (TSct.look_s(TID[_c_val].getname()) == true) { throw SemErr("second structure initialisation");}
	TSct.print_TSct();
	TSct.new_s(TID[_c_val].getname());
	TSct.print_TSct();
	gl();
	if (_c_type != LEX_LBRACE) { throw _c_lex;}
	gl();
	while (_c_type == LEX_INT || _c_type == LEX_STRING || _c_type == LEX_BOOL || _c_type == LEX_REAL || _c_type == LEX_STRUCT)
	{
		type();
		if (_c_type != LEX_ID) { throw _c_lex;}
		if  (TSct.look_els(TID[_c_val].getname()) == true) { throw SemErr("second initialisation in a structure");}
		if (LTID.get_lextype() == LEX_STRUCT) { throw SemErr("struct in struct");}
		TSct.new_els(TID[_c_val].getname(), LTID.get_lextype());
		gl();
		while (_c_type == LEX_COMMA)
		{
			gl();
			if (_c_type != LEX_ID) { throw _c_lex;}
			if (TSct.look_els(TID[_c_val].getname()) == true) { throw SemErr("second initialization in a structure");}
			TSct.new_els(TID[_c_val].getname(), LTID.get_lextype());
			gl();
		}
		LTID.set_lextype(LEX_NULL);
		if (_c_type != LEX_SEMICOLON) { throw _c_lex;}
		gl();
	}
	if (_c_type != LEX_RBRACE) { throw _c_lex;}
	gl();
	LTID.set_lextype(LEX_STRUCT);
	if (_c_type == LEX_ID)
	{
		if (LTID.look_id(TID[_c_val].getname()) == true) { throw SemErr("second structure 'id' initialization");} 
		LTID.new_id(TID[_c_val].getname());
		gl();
		while (_c_type == LEX_COMMA)
		{
			gl();
			if (_c_type != LEX_ID) { throw _c_lex;}
			if (LTID.look_id(TID[_c_val].getname()) == true) { throw SemErr("second structure 'id' initialization"); }
			LTID.new_id(TID[_c_val].getname());
			gl();
		}
	}
	LTID.set_lextype(LEX_NULL);
	cout << "_______strct() : end" << endl;
}

void Parser::body()
{
	cout << "_______body() : begin" << endl;
	if (_c_type != LEX_START) { throw _c_lex;}
	gl();
//	while (_c_type == LEX_INT || _c_type == LEX_STRING || _c_type == LEX_BOOL || _c_type == LEX_REAL || _c_type == LEX_STRUCT || _c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS || _c_type == LEX_IF || _c_type == LEX_CASE || _c_type == LEX_WHILE || _c_type == LEX_DO || _c_type == LEX_FOR || _c_type == LEX_READ || _c_type == LEX_WRITE || _c_type == LEX_GOTO || _c_type == LEX_CONTINUE || _c_type == LEX_BREAK )
	while ((_c_type >= 5) && (_c_type <= 16) || (_c_type >= 19) && (_c_type <= 31) || _c_type == LEX_LPAREN )
	{
	//	if (_c_type == LEX_INT || _c_type == LEX_STRING || _c_type == LEX_BOOL || _c_type == LEX_REAL || _c_type == LEX_STRUCT)
		if ((_c_type >= 5) && (_c_type <= 9))
		{
			definition();
			_prog.putlex(_c_lex);
			if (_c_type != LEX_SEMICOLON) { throw _c_lex;}
			gl();
		}
	//	if (_c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS || _c_type == LEX_IF || _c_type == LEX_CASE || _c_type == LEX_WHILE || _c_type == LEX_DO || _c_type == LEX_FOR || _c_type == LEX_READ || _c_type == LEX_WRITE || _c_type == LEX_GOTO || _c_type == LEX_CONTINUE || _c_type == LEX_BREAK)
		if ((_c_type >= 10) && (_c_type <= 16) || (_c_type >= 19) && (_c_type <= 31) || _c_type == LEX_LPAREN)
		{
			instruction();
			if (_c_type != LEX_SEMICOLON) { throw _c_lex;}
			gl();
		}
	}
	while (_c_type == LEX_START)
	{
		LTID.new_t();
		body();
		LTID.del_t();
	//	while (_c_type == LEX_INT || _c_type == LEX_STRING || _c_type == LEX_BOOL || _c_type == LEX_REAL || _c_type == LEX_STRUCT || _c_type == LEX_STRUCTURE || _c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_LPAREN  || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS || _c_type == LEX_IF || _c_type == LEX_CASE || _c_type == LEX_WHILE || _c_type == LEX_DO || _c_type == LEX_FOR || _c_type == LEX_READ || _c_type == LEX_WRITE || _c_type == LEX_GOTO || _c_type == LEX_CONTINUE || _c_type == LEX_BREAK)
		while ((_c_type >= 5) && (_c_type <= 16) || (_c_type >= 19) && (_c_type <= 31) || _c_type == LEX_LPAREN )
		{
		//	if (_c_type == LEX_INT || _c_type == LEX_STRING || _c_type == LEX_BOOL || _c_type == LEX_REAL || _c_type == LEX_STRUCT)
			if ((_c_type >= 5) && (_c_type <= 9))
			{
				definition();
				_prog.putlex(_c_lex);
				if (_c_type != LEX_SEMICOLON) { throw _c_lex;}
				gl();
			}
		//	if (_c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS || _c_type == LEX_IF || _c_type == LEX_CASE || _c_type == LEX_WHILE || _c_type == LEX_DO || _c_type == LEX_FOR || _c_type == LEX_READ || _c_type == LEX_WRITE || _c_type == LEX_GOTO || _c_type == LEX_CONTINUE || _c_type == LEX_BREAK)
			if ((_c_type >= 10) && (_c_type <= 16) || (_c_type >= 19) && (_c_type <= 31) || _c_type == LEX_LPAREN)
			{
				instruction();
				if (_c_type != LEX_SEMICOLON) { throw _c_lex;}
				gl();
			}
		}
	}
	if (_c_type != LEX_FINISH) { throw _c_lex;}
	gl();
	cout << "_______body() : end" << endl;
}

void Parser::definition()
{
	cout << "_______definition() : begin" << endl;
	type();
	variable();
	while (_c_type == LEX_COMMA)
	{
		_prog.putlex( Lex(LEX_SEMICOLON) );
		gl();
		variable();
	}
	cout << "_______definition() : end" << endl;
}

void Parser::type()
{
	cout << "_______type() : begin" << endl;
	if (_c_type != LEX_INT && _c_type != LEX_STRING && _c_type != LEX_BOOL && _c_type != LEX_REAL && _c_type != LEX_STRUCT) { throw _c_lex;}
	LTID.set_lextype(_c_type);
	if (_c_type == LEX_STRUCT)
	{
		gl();
		if (_c_type != LEX_ID) { throw _c_lex;}
		LTID.set_structname(TID[_c_val].getname());
		TSct.print_TSct();
		if (TSct.look_s(TID[_c_val].getname()) == false) { throw SemErr("undeclared structure");}
	}
	gl();
	cout << "_______type() : end" << endl;
}

void Parser::variable()
{
	int i = 0;
	cout << "_______variable() : begin" << endl;
	if (_c_type != LEX_ID) { throw _c_lex;}
	const char* s = TID[_c_val].getname();
	if (LTID.look_id(s) == true) { throw SemErr("second initialization in definition");}
	LTID.new_id(s);
	lextype type = LTID.get_typeid(s);
	_st_lex.push(type);
	if (type == LEX_STRUCT)
	{
		_st_name.push(LTID.get_idstructname(s));
	}
	s = 0;
	gl();
	if (_c_type == LEX_ASSIGN)
	{
		_prog.putlex( Lex(POLIZ_ADDRESS,_p_val) );
		_st_ops.push(_c_lex);
		i = 1;
		_st_lex.push(LEX_ASSIGN);
		gl();
		while (_c_type == LEX_ID)
		{
			const char* s = TID[_c_val].getname();
			if (LTID.look_alltables(s) == false) {
				 throw SemErr("undeclared 'id' in a definition");}
			lextype type = LTID.get_typeid(s);
			_st_lex.push(type);
			if (type == LEX_STRUCT)
			{
				_st_name.push(LTID.get_idstructname(s));
			}
			gl();
			if (_c_type != LEX_ASSIGN)
			{
				ungl();
				_st_lex.pop();
				if (LTID.get_typeid(TID[_c_val].getname()) == LEX_STRUCT)
				{
					_st_name.pop();
				}
				break;
			}
			_prog.putlex( Lex(POLIZ_ADDRESS,_p_val) );
			_st_ops.push(_c_lex);
			LTID.cmp_three();
			_st_lex.push(LEX_ASSIGN);
			gl();
		}
	}
	else
	{
		ungl();
		_st_lex.pop();
		if (LTID.get_typeid(TID[_c_val].getname()) == LEX_STRUCT)
		{
			_st_name.pop();
		}
	}
/*		LTID.cmp_three();
		s = 0;
			gl();
			if (_c_type == LEX_ASSIGN) { throw _c_lex;}
			_st_lex.push(LEX_ASSIGN);
			gl();
		}
*/	action();
	if ( _st_ops.isempty() == false)
	{
		lextype t = _st_ops.peek().gettype();
		while ( _st_ops.isempty() == false && t == LEX_ASSIGN )
		{
			_prog.putlex(_st_ops.pop());
			if ( _st_ops.isempty() == false)
				t = _st_ops.peek().gettype();
		}
	}
	if (i)
	{
		LTID.cmp_three();
	}
	cout << "_______variable() : end" << endl;
}

void Parser::instruction()
{
	cout << "_______instruction() : begin" << endl;
/*&	if (_c_type == LEX_ID)
	{
		gl();
		if (_c_type == LEX_COLON)
		{
			ungl();
			jump();
		}
		else
		{
			ungl();
			expression();
		}
	}
	else
	if (_c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS)
*/	if (_c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS)
	{
		expression();
		_prog.putlex(_c_lex);
	}
	else
	if (_c_type == LEX_IF || _c_type == LEX_CASE)
	{
		stipulation();
	}
	else
	if (_c_type == LEX_WHILE || _c_type == LEX_DO || _c_type == LEX_FOR)
	{
		cycle();
	}
	else
	if (_c_type == LEX_READ || _c_type == LEX_WRITE)
	{
		input_output();
	}
	else
	if (_c_type == LEX_GOTO || _c_type == LEX_CONTINUE || _c_type == LEX_BREAK)
	{
		jump();
	}
	else { throw _c_lex;}
	//if (_c_type != LEX_SEMICOLON) { throw _c_lex;}
	cout << "_______instruction() : end" << endl;
}

void Parser::stipulation()
{
	cout << "_______stipulation() : begin" << endl;
	if (_c_type == LEX_IF)
	{
		int L2, L3;
		gl();
		if (_c_type != LEX_LPAREN) { throw _c_lex;}
		gl();
		expression();
		LTID.cmp_bool();
		if (_c_type != LEX_RPAREN) { throw _c_lex;}
		gl();
		L2 = _prog.getfree();
		_prog.blank();
		_prog.putlex( Lex(POLIZ_FGO) );
		if (_c_type == LEX_START || ((_c_type >= 5) && (_c_type <= 9)) ||  _c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS || _c_type == LEX_IF || _c_type == LEX_CASE || _c_type == LEX_WHILE || _c_type == LEX_DO || _c_type == LEX_FOR || _c_type == LEX_READ || _c_type == LEX_WRITE || _c_type == LEX_GOTO || _c_type == LEX_CONTINUE || _c_type == LEX_BREAK)
	//	if (_c_type == LEX_START || _c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS)
		{
			quant_expr();
		}
		if (_c_type != LEX_SEMICOLON) { throw _c_lex;}
		gl();
		L3 = _prog.getfree();
		_prog.blank();
		_prog.putlex( Lex(POLIZ_GO) );
		_prog.putlex( Lex(POLIZ_LABEL,_prog.getfree()), L2 );
		if (_c_type == LEX_ELSE)
		{
			gl();
			if (_c_type == LEX_START || ((_c_type >= 5) && (_c_type <= 9)) || _c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS || _c_type == LEX_IF || _c_type == LEX_CASE || _c_type == LEX_WHILE || _c_type == LEX_DO || _c_type == LEX_FOR || _c_type == LEX_READ || _c_type == LEX_WRITE || _c_type == LEX_GOTO || _c_type == LEX_CONTINUE || _c_type == LEX_BREAK)
		//	<F2>if (_c_type == LEX_START || _c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS)
			{
				quant_expr();
			}
			if (_c_type != LEX_SEMICOLON) { throw _c_lex;}
			gl();			
		}
		_prog.putlex( Lex(POLIZ_LABEL,_prog.getfree()), L3 );
	}
	else
	if (_c_type == LEX_CASE)
	{
		gl();
		if (_c_type != LEX_LPAREN) { throw _c_lex;}
		gl();
		expression();
		LTID.cmp_int();
		if (_c_type != LEX_RPAREN) { throw _c_lex;}
		gl();
		if (_c_type != LEX_OF) { throw _c_lex;}
		gl();
		variants();
		if (_c_type != LEX_END) { throw _c_lex;}
		gl();
	}
	else { throw _c_lex;}
	cout << "_______stipulation() : end" << endl;
}

void Parser::cycle()
{
	cout << "_______cycle() : begin" << endl;
	if (_c_type == LEX_WHILE)
	{
		int L0 = _prog.getfree(), L1;
		Tends t;
		t.p = NULL;
		_st_labels.push(L0);
		_st_ends.push(t);
		gl();
		if (_c_type != LEX_LPAREN) { throw _c_lex;}
		gl();
		expression();
		LTID.cmp_bool();
		if (_c_type != LEX_RPAREN) { throw _c_lex;}
		gl();
		L1 = _prog.getfree();
		_prog.blank();
		_prog.putlex( Lex(POLIZ_FGO));
		
		if (_c_type == LEX_START || ((_c_type >= 5) && (_c_type <= 9)) || _c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS || _c_type == LEX_IF || _c_type == LEX_CASE || _c_type == LEX_WHILE || _c_type == LEX_DO || _c_type == LEX_FOR || _c_type == LEX_READ || _c_type == LEX_WRITE || _c_type == LEX_GOTO || _c_type == LEX_CONTINUE || _c_type == LEX_BREAK)
	//	if (_c_type == LEX_START || _c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS)
		{
			quant_expr();
		}
		if (_c_type != LEX_SEMICOLON) { throw _c_lex;}
		gl();
		_prog.putlex( Lex(POLIZ_LABEL,L0) );
		_prog.putlex( Lex(POLIZ_GO) );
		int L = _prog.getfree();
		_prog.putlex( Lex(POLIZ_LABEL,L), L1 );
		t = _st_ends.peek();
		while (t.p != NULL)
		{
			_prog.putlex( Lex(POLIZ_LABEL,L), (t.p)->label );
			_ends* tmp = (t.p)->next;
			delete t.p;
			t.p = tmp;
		}
		_st_labels.pop();
		_st_ends.pop();
	}
	else
	if (_c_type == LEX_DO)
	{
		int L0 = _prog.getfree(), L1;
		Tends t;
		t.p = NULL;
		_st_labels.push(L0);
		_st_ends.push(t);
		gl();
		quant_expr();
		if (_c_type != LEX_SEMICOLON) { throw _c_lex;}
		gl();
		if (_c_type != LEX_WHILE) { throw _c_lex;}
		gl();
		if (_c_type != LEX_LPAREN) { throw _c_lex;}
		gl();
		expression();
		LTID.cmp_bool();
		if (_c_type != LEX_RPAREN) { throw _c_lex;}
		gl();
		L1 = _prog.getfree();
		_prog.blank();
		_prog.putlex( Lex(POLIZ_FGO));
		_prog.putlex( Lex(POLIZ_LABEL,L0) );
		_prog.putlex( Lex(POLIZ_GO) );
		int L = _prog.getfree();
		_prog.putlex( Lex(POLIZ_LABEL,L), L1 );
		t = _st_ends.peek();
		while (t.p != NULL)
		{
			_prog.putlex( Lex(POLIZ_LABEL,L), (t.p)->label );
			_ends* tmp = (t.p)->next;
			delete t.p;
			t.p = tmp;
		}
		_st_labels.pop();
		_st_ends.pop();
	}
	else
	if (_c_type == LEX_FOR)
	{
		gl();
		if (_c_type != LEX_LPAREN) { throw _c_lex;}
		gl();
		if (_c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS)
		{
			expression();
			_prog.putlex(_c_lex);
		}
		if (_c_type != LEX_SEMICOLON) { throw _c_lex;}
		gl();
		int L0 = _prog.getfree(), L1, L2, L3;
		Tends t;
		t.p = NULL;
		_st_labels.push(L0);
		_st_ends.push(t);
		if (_c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS)
		{
			expression();
			LTID.cmp_bool();
		}
		else
		{
			_prog.putlex( Lex(LEX_TRUE,1) );
		}
		if (_c_type != LEX_SEMICOLON) { throw _c_lex;}
		gl();
		L1 = _prog.getfree();
		_prog.blank();
		_prog.putlex( Lex(POLIZ_FGO));
		L2 = _prog.getfree();
		_prog.blank();
		_prog.putlex( Lex(POLIZ_GO));
		L3 = _prog.getfree();
		if (_c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS)
		{
			expression();
			_prog.putlex(Lex(LEX_SEMICOLON));
		}
		if (_c_type != LEX_RPAREN) { throw _c_lex;}
		gl();
		_prog.putlex( Lex(POLIZ_LABEL,L0) );
		_prog.putlex( Lex(POLIZ_GO) );
		_prog.putlex( Lex(POLIZ_LABEL,_prog.getfree()), L2 );
		quant_expr();
		if (_c_type != LEX_SEMICOLON) { throw _c_lex;}
		gl();
		_prog.putlex( Lex(POLIZ_LABEL,L3) );
		_prog.putlex( Lex(POLIZ_GO) );
		int L = _prog.getfree();
		_prog.putlex( Lex(POLIZ_LABEL,L), L1 );
		t = _st_ends.peek();
		while (t.p != NULL)
		{
			_prog.putlex( Lex(POLIZ_LABEL,L), (t.p)->label );
			_ends* tmp = (t.p)->next;
			delete t.p;
			t.p = tmp;
		}
		_st_labels.pop();
		_st_ends.pop();
	}
	else { throw _c_lex;}
	cout << "_______cycle() : end" << endl;
}

void Parser::input_output()
{
	cout << "_______input_output() : begin" << endl;
	if (_c_type == LEX_READ)
	{
		gl();
		if (_c_type != LEX_LPAREN) { throw _c_lex;}
		gl();
		if (_c_type != LEX_ID) { throw _c_lex;}
		if (LTID.look_alltables(TID[_c_val].getname()) == false) { throw SemErr("undeclared 'id'");}
		_prog.putlex( Lex(POLIZ_ADDRESS,_c_val) );
		gl();
		if (_c_type != LEX_RPAREN) { throw _c_lex;}
		gl();
		_prog.putlex( Lex(LEX_READ) );
	}
	else
	if (_c_type == LEX_WRITE)
	{
		gl();
		if (_c_type != LEX_LPAREN) { throw _c_lex;}
		gl();
		expression();
		while (_c_type == LEX_COMMA)
		{
			gl();
			expression();
		}
		if (_c_type != LEX_RPAREN) { throw _c_lex;}
		gl();
		_prog.putlex( Lex(LEX_WRITE) );
	}
	else { throw _c_lex;}
	cout << "_______input_output() : end" << endl;
}

void Parser::jump()
{
	cout << "_______jump() : begin" << endl;
/*	if (_c_type == LEX_ID)
	{
		tagged_instr();
	}
	else
	if (_c_type == LEX_GOTO)
	{
		gl();
		if (_c_type != LEX_ID) { throw _c_lex;}
		gl();
	}
	else
*/	if (_c_type == LEX_CONTINUE)
	{
		gl();
		if ( _st_labels.isempty() ) { throw SemErr("'continue' is out of the loop");}
		_prog.putlex( Lex( POLIZ_LABEL, _st_labels.peek() ) );
		_prog.putlex( Lex(POLIZ_GO) );
	}
	else if (_c_type == LEX_BREAK)
	{
		gl();
		if ( _st_labels.isempty() ) { throw SemErr("'break' is out of the loop");}
		Tends t = _st_ends.pop();
		if (t.p == NULL)
		{
			t.p = new _ends;
			(t.p)->next = NULL;
			(t.p)->label = _prog.getfree();
			_st_ends.push(t);
		}
		else
		{
			_ends* tmp = t.p;
			while (tmp->next != NULL)
				tmp = tmp->next;
			tmp->next = new _ends;
			tmp->next->next = NULL;
			tmp->next->label = _prog.getfree();
			_st_ends.push(t);
		}
		_prog.blank();
		_prog.putlex( Lex(POLIZ_GO));
	}
	else { throw _c_lex;}
	cout << "_______jump() : end" << endl;
}
/*
void Parser::tagged_instr()
{
	cout << "_______tagged_instr() : begin" << endl;
	if (_c_type != LEX_ID) { throw _c_lex;}
	gl();
	if (_c_type != LEX_COLON) { throw _c_lex;}
	gl();
	instruction();
	cout << "_______tagged_instr() : end" << endl;
}
*/
void Parser::expression()
{
	int i = 0;
	cout << "_______expression() : begin" << endl;
	if (_c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS)
	{
		action();
	}
	else
	if (_c_type == LEX_ID || _c_type == LEX_STRUCTURE)
	{
		const char* s = TID[_c_val].getname();
		if (LTID.look_alltables(s) == false) { throw SemErr("undeclared 'id'");}
		lextype type = LTID.get_typeid(s);
		_st_lex.push(type);
		if (type == LEX_STRUCT)
		{
			_st_name.push(LTID.get_idstructname(s));
		}
		gl();
		if (_c_type == LEX_ASSIGN)
		{
			_prog.putlex( Lex(POLIZ_ADDRESS,_p_val) );
			_st_ops.push(_c_lex);
			const char* s = TID[_c_val].getname();
			i = 1;
			_st_lex.push(LEX_ASSIGN);
			gl();
			while (_c_type == LEX_ID || _c_type == LEX_STRUCTURE)
			{
				const char* s = TID[_c_val].getname();
				if (LTID.look_alltables(s) == false) { throw SemErr("undeclared 'id'");}
				lextype type = LTID.get_typeid(s);
				_st_lex.push(type);
				if (type == LEX_STRUCT)
				{
					_st_name.push(LTID.get_idstructname(s));
				}
				gl();
				if (_c_type != LEX_ASSIGN)
				{
					ungl();
					_st_lex.pop();
					if (LTID.get_typeid(TID[_c_val].getname()) == LEX_STRUCT)
					{
						_st_name.pop();
					}
					break;
				}
				_prog.putlex( Lex(POLIZ_ADDRESS,_p_val) );
				_st_ops.push(_c_lex);
				LTID.cmp_three();
				_st_lex.push(LEX_ASSIGN);
				gl();
			}
		}
		else
		{
			ungl();
			_st_lex.pop();
			if (LTID.get_typeid(TID[_c_val].getname()) == LEX_STRUCT)
			{
				_st_name.pop();
			}
		}
		action();
		if ( _st_ops.isempty() == false)
		{
			lextype t = _st_ops.peek().gettype();
			while ( _st_ops.isempty() == false && t == LEX_ASSIGN )
			{
				_prog.putlex(_st_ops.pop());
				if ( _st_ops.isempty() == false)
					t = _st_ops.peek().gettype();
			}
		}
		if (i)
		{
			LTID.cmp_three();
		}
	}
	else { throw _c_lex;}
	cout << "_______expression() : end" << endl;
}

void Parser::action()
{
	cout << "_______action() : begin" << endl;
	bool_sum();
	while (_c_type == LEX_OR)
	{
		_st_lex.push(_c_type);
//		_st_ops.push(_c_lex);
		gl();
		bool_sum();
		_prog.putlex( Lex(LEX_OR) );//!!!!!!!!!!
		LTID.cmp_three();
	}
/*	if ( _st_ops.isempty() == false)
	{
		lextype t = _st_ops.peek().gettype();
		while ( _st_ops.isempty() == false && t == LEX_OR )
		{
			_prog.putlex(_st_ops.pop());
			if ( _st_ops.isempty() == false)
				t = _st_ops.peek().gettype();
		}
	}
*/	cout << "_______action() : end" << endl;
}

void Parser::bool_sum()
{
	cout << "_______bool_sum() : begin" << endl;
	bool_mul();
	while (_c_type == LEX_AND)
	{
		_st_lex.push(_c_type);
//		_st_ops.push(_c_lex);
		gl();
		bool_mul();
		_prog.putlex( Lex(LEX_AND) );//!!!!!!!!!!
		LTID.cmp_three();
	}
/*	if ( _st_ops.isempty() == false)
	{
		lextype t = _st_ops.peek().gettype();
		while ( _st_ops.isempty() == false  && t == LEX_AND )
		{
			_prog.putlex(_st_ops.pop());
			if ( _st_ops.isempty() == false)
				t = _st_ops.peek().gettype();
		}
	}
*/	cout << "_______bool_sum() : end" << endl;
}

void Parser::bool_mul()
{
	cout << "_______bool_mul() : begin" << endl;
	cmp_variable();
	Lex l;
	if (_c_type == LEX_LSS ||_c_type == LEX_GTR ||_c_type == LEX_LEQ || _c_type == LEX_GEQ || _c_type == LEX_EQ || _c_type == LEX_NEQ)
	{
		l = _c_lex;
		_st_lex.push(_c_type);
		gl();
		cmp_variable();
		LTID.cmp_three();
	}
	if (l.gettype() != LEX_NULL)
		_prog.putlex(l);
	cout << "_______bool_mul() : end" << endl;
}

void Parser::cmp_variable()
{
	cout << "_______cmp_variable() : begin" << endl;
	sum();
	while (_c_type == LEX_PLUS || _c_type == LEX_MINUS)
	{
		Lex l = _c_lex;//!!!!!!!!!!
		_st_lex.push(_c_type);
//		_st_ops.push(_c_lex);
		gl();
		sum();
		_prog.putlex(l);//!!!!!!!!!!
		LTID.cmp_three();
	}
/*	if ( _st_ops.isempty() == false)
	{
		lextype t = _st_ops.peek().gettype();
		while ( _st_ops.isempty() == false  && (t == LEX_PLUS || t == LEX_MINUS) )
		{
			_prog.putlex(_st_ops.pop());
			if ( _st_ops.isempty() == false)
				t = _st_ops.peek().gettype();
		}
	}
*/	cout << "_______cmp_variable() : end" << endl;
}

void Parser::sum()
{
	cout << "_______sum() : begin" << endl;
	mul_with_sign();
	while (_c_type == LEX_TIMES || _c_type == LEX_SLASH || _c_type == LEX_PERCENT)
	{
		Lex l = _c_lex;//!!!!!!!!!!
		_st_lex.push(_c_type);
//		_st_ops.push(_c_lex);
		gl();
		mul_with_sign();
		_prog.putlex(l);//!!!!!!!!!!
		LTID.cmp_three();
	}
/*	if ( _st_ops.isempty() == false)
	{
		lextype t = _st_ops.peek().gettype();
		while ( _st_ops.isempty() == false  && (t == LEX_TIMES || t == LEX_SLASH || t == LEX_PERCENT) )
		{
			_prog.putlex(_st_ops.pop());
			if ( _st_ops.isempty() == false)
				t = _st_ops.peek().gettype();
		}
	}
*/	cout << "_______sum() : end" << endl;
}

void Parser::mul_with_sign()
{
	cout << "_______mul_with_sign() : begin" << endl;
	int i = 0;
	int k = 0;
	if (_c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS)
	{
		i = 1;
		_st_lex.push(_c_type);
		if (_c_type == LEX_NOT)
			_st_ops.push(_c_lex);
		else if (_c_type == LEX_MINUS)
			k = 1;
		gl();
		while (_c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS)
		{
			if (_c_type == LEX_NOT)
				_st_ops.push(_c_lex);
			else if (_c_type == LEX_MINUS)
				k = (k + 1) % 2;
			_st_lex.push(_c_type);
			gl();
			LTID.cmp_two();
		}
	}
	if (k)
		_prog.putlex( Lex(LEX_NUM,0) );
	mul();
	if (k)
		_prog.putlex( Lex(LEX_MINUS) );
	if ( _st_ops.isempty() == false)
	{
		lextype t = _st_ops.peek().gettype();
		while ( _st_ops.isempty() == false  && t == LEX_NOT )
		{
			_prog.putlex(_st_ops.pop());
			if ( _st_ops.isempty() == false)
				t = _st_ops.peek().gettype();
		}
	}		
	if (i)
	{
		LTID.cmp_two();
	}
	cout << "_______mul_with_sign() : end" << endl;
}

void Parser::mul()
{
	cout << "_______mul() : begin" << endl;
	if (_c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE)
	{
		if (_c_type == LEX_ID || _c_type == LEX_STRUCTURE)
		{
			const char* s = TID[_c_val].getname();
			if (LTID.look_alltables(s) == false) { throw SemErr("undeclared 'id'");}
			lextype type = LTID.get_typeid(s);
			_st_lex.push(type);
			if (type == LEX_STRUCT)
			{
				_prog.putlex( Lex(POLIZ_ADDRESS,_c_val) );
				_st_name.push(LTID.get_idstructname(s));
			}
			_prog.putlex( Lex(LEX_ID,_c_val) );
		}
		else
		{
			_st_lex.push(_c_type);
			if (_c_type == LEX_STRUCT)
				_prog.putlex( Lex(POLIZ_ADDRESS,_c_val) );
			else
				_prog.putlex( _c_lex );
		}
		gl();
	}
	else
	if (_c_type == LEX_LPAREN)
	{
		gl();
		action();
		if (_c_type != LEX_RPAREN) { throw _c_lex;}
		gl();
	}
	else { throw _c_lex;}
	cout << "_______mul() : end" << endl;
}

void Parser::quant_expr()
{
	cout << "_______quant_expr() : begin" << endl;
	if (_c_type == LEX_START)
	{
		LTID.new_t();
		body();
		LTID.del_t();
	}
	else
	if (_c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS || _c_type == LEX_IF || _c_type == LEX_CASE || _c_type == LEX_WHILE || _c_type == LEX_DO || _c_type == LEX_FOR || _c_type == LEX_READ || _c_type == LEX_WRITE || _c_type == LEX_GOTO || _c_type == LEX_CONTINUE || _c_type == LEX_BREAK)
//	if (_c_type == LEX_ID || _c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE || _c_type == LEX_LPAREN || _c_type == LEX_NOT || _c_type == LEX_PLUS || _c_type == LEX_MINUS)
	{
		LTID.new_t();
		instruction();
		LTID.del_t();
//		if (_c_type != LEX_SEMICOLON) { throw _c_lex;}
//		gl();
	}
	else
	if ((_c_type >= 5) && (_c_type <= 9))
	{
		definition();
	}
	else { throw _c_lex;}
	cout << "_______quant_expr() : end" << endl;
}

void Parser::variants()
{
	cout << "_______variants() : begin" << endl;
	variant();
	while (_c_type == LEX_NUM || _c_type == LEX_TRUE || _c_type == LEX_FALSE || _c_type == LEX_DOUBLE || _c_type == LEX_STR || _c_type == LEX_STRUCTURE)
	{
		variant();
	}
	cout << "_______variants() : end" << endl;
}

void Parser::variant()
{
	cout << "_______variant() : begin" << endl;
//	if (_c_type != LEX_NUM && _c_type != LEX_TRUE && _c_type != LEX_FALSE && _c_type != LEX_STR) { throw _c_lex;}
	if (_c_type != LEX_NUM) { throw _c_lex;}
	gl();
	while (_c_type == LEX_COMMA)
	{
		gl();
	//	if (_c_type != LEX_NUM && _c_type != LEX_TRUE && _c_type != LEX_FALSE && _c_type != LEX_STR) { throw _c_lex;}
		if (_c_type != LEX_NUM) { throw _c_lex;}
		gl();
	}
	if (_c_type != LEX_COLON) { throw _c_lex;}
	gl();
	instruction();
	cout << "_______variant() : end" << endl;
}

struct write
{
	arg a;
	write* next;
};

class Executer
{
	Lex _currlex;
public:
	void execute(Poliz& prog)
	{
		Stack <arg,100> args;
		int index;
		int size = prog.getfree();
		for (index = 0; index < size; index++)
		{
			_currlex = prog[index];
			switch (_currlex.gettype())
			{
				case LEX_TRUE:
					args.push( arg( 1, _currlex.gettype() ) );
					break;
				case LEX_FALSE:
					args.push( arg( 0, _currlex.gettype() ) );
					break;
				case LEX_NUM:
				case LEX_DOUBLE:
				case LEX_STR:
				case POLIZ_LABEL:
				case POLIZ_ADDRESS:
					args.push( arg( _currlex.getvalue(), _currlex.gettype() ) );
					break;
				case LEX_ID:
				{
						int i = LTID.get_value( TID[ _currlex.getvalue() ].getname() );
						lextype t = LTID.get_typeid( TID[ _currlex.getvalue() ].getname() );
						switch (t)
						{
							case LEX_INT:
								args.push( arg( i, LEX_NUM ) );
								break;
							case LEX_BOOL:
								if (i)
									args.push( arg( true, LEX_TRUE ) );
								else
									args.push( arg( false, LEX_FALSE ) );
								break;
							case LEX_REAL:
								args.push( arg( i, LEX_DOUBLE ) );
								break;
							case LEX_STRING:
								args.push( arg( i, LEX_STR ) );
								break;
							case LEX_STRUCT:
								args.push( arg( i, LEX_STRUCT ) );
								break;
							default:
	cerr << t << endl;
								throw ExecErr("*** EXECUTION ERROR: wrong type of identifier ***");
						}
				}
					break;
				case LEX_NOT:
				{
					bool i = !args.pop()._v.i;
					if (i)
						args.push( arg( true, LEX_TRUE ) );
					else
						args.push( arg( false, LEX_FALSE ) );
				}
					break;
				case LEX_OR:
				{
					bool i = args.pop()._v.i || args.pop()._v.i;
					if (i)
						args.push( arg( true, LEX_TRUE ) );
					else
						args.push( arg( false, LEX_FALSE ) );
				}
					break;
				case LEX_AND:
				{
					bool i = args.pop()._v.i && args.pop()._v.i;
					if (i)
						args.push( arg( true, LEX_TRUE ) );
					else
						args.push( arg( false, LEX_FALSE ) );
				}
					break;
				case POLIZ_GO:
					index = args.pop()._v.i - 1;
					break;
				case POLIZ_FGO:
				{
					int i = args.pop()._v.i - 1;
					if ( !args.pop()._v.i )
						index = i;
				}
					break;
				case LEX_WRITE:
				{
					write* p = new write;
					p->a = args.pop();
					p->next = NULL;
					while (args.isempty() == false)
					{
						write* t = new write;
						t->a = args.pop();
						t->next = p;
						p = t;
					}
					while (p != NULL)
					{
						write* t = p->next;
						cerr << p->a;
						delete p;
						p = t;
					}
					cerr << endl;
				}
					break;
				case LEX_READ:
				{
					arg a = args.pop();
					switch ( LTID.get_typeid( TID[a._v.i].getname() ) )
					{
						case LEX_INT:
						{
							int i;
							cin >> i;
							LTID.put_value( TID[a._v.i].getname(), i );
						}
							break;
						case LEX_BOOL:
						{
							char s[8];
							cin >> s;
							if (strcmp(s,"true") == 0 || strcmp(s,"1") == 0)
								LTID.put_value( TID[a._v.i].getname(), 1);
							else if (strcmp(s,"false") == 0 || strcmp(s,"0") == 0)
								LTID.put_value( TID[a._v.i].getname(), 0);
							else throw ExecErr("*** EXECUTION ERROR: not a bool expression (expected: true/false/1/0) ***");
						}
							break;
						case LEX_REAL:
						{
							double r;
							cin >> r;
							if ( LTID.get_assign( TID[a._v.i].getname() ) )
							{
								TR[LTID.get_value( TID[a._v.i].getname() )] = r;
							}
							else
							{
								int i = TR.put(r);
								LTID.put_value( TID[a._v.i].getname(), i );
							}
						}
							break;
						case LEX_STRING:
						{
							char s[32];
							cin >> s;
							if ( LTID.get_assign( TID[a._v.i].getname() ) )
							{
								TS[LTID.get_value( TID[a._v.i].getname() )] = s;
							}
							else
							{
								int i = TS.put(s);
								LTID.put_value( TID[a._v.i].getname(), i );
							}
						}
							break;
					}
					LTID.put_assign( TID[a._v.i].getname() );
				}
					break;
				case LEX_PLUS:
					args.push( args.pop() + args.pop() );
					break;
				case LEX_TIMES:
					args.push( args.pop() * args.pop() );
					break;
				case LEX_MINUS:
				{
					arg i = args.pop();
					args.push( args.pop() - i );
				}
					break;
				case LEX_SLASH:
				{
					arg i = args.pop();
					args.push( args.pop() / i );
				}
					break;
				case LEX_PERCENT:
				{
					arg i = args.pop();
					args.push( args.pop() % i );
				}
					break;
				case LEX_EQ:
				{
					bool i;
					arg y = args.pop();
					arg x = args.pop();
					if (x._t == LEX_NUM && y._t == LEX_NUM)
						i = x._v.i == y._v.i;
					else if (x._t == LEX_NUM && y._t == LEX_DOUBLE)
						i = x._v.i == TR[ y._i ];
					else if (x._t == LEX_DOUBLE && y._t == LEX_NUM)
						i = TR[ x._i ] == y._v.i;
					else if (x._t == LEX_DOUBLE && y._t == LEX_DOUBLE)
						i = TR[ x._i ] == TR[ y._i ];
					else if (x._t == LEX_STR && y._t == LEX_STR)
						i = (strcmp( TS[ x._i ], TS[ y._i ] ) == 0);
					else throw ExecErr("*** EXECUTION ERROR: wrong types on operator == ***");
					if (i)
						args.push( arg( true, LEX_TRUE ) );
					else
						args.push( arg( false, LEX_FALSE ) );
				}
					break;
				case LEX_LSS:
				{
					bool i;
					arg y = args.pop();
					arg x = args.pop();
					if (x._t == LEX_NUM && y._t == LEX_NUM)
						i = x._v.i < y._v.i;
					else if (x._t == LEX_NUM && y._t == LEX_DOUBLE)
						i = x._v.i < TR[ y._i ];
					else if (x._t == LEX_DOUBLE && y._t == LEX_NUM)
						i = TR[ x._i ] < y._v.i;
					else if (x._t == LEX_DOUBLE && y._t == LEX_DOUBLE)
						i = TR[ x._i ] < TR[ y._i ];
					else if (x._t == LEX_STR && y._t == LEX_STR)
						i = (strcmp( TS[ x._i ], TS[ y._i ] ) < 0);
					else throw ExecErr("*** EXECUTION ERROR: wrong types on operator < ***");
					if (i)
						args.push( arg( true, LEX_TRUE ) );
					else
						args.push( arg( false, LEX_FALSE ) );
				}
					break;
				case LEX_GTR:
				{
					bool i;
					arg y = args.pop();
					arg x = args.pop();
					if (x._t == LEX_NUM && y._t == LEX_NUM)
						i = x._v.i > y._v.i;
					else if (x._t == LEX_NUM && y._t == LEX_DOUBLE)
						i = x._v.i > TR[ y._i ];
					else if (x._t == LEX_DOUBLE && y._t == LEX_NUM)
						i = TR[ x._i ] > y._v.i;
					else if (x._t == LEX_DOUBLE && y._t == LEX_DOUBLE)
						i = TR[ x._i ] > TR[ y._i ];
					else if (x._t == LEX_STR && y._t == LEX_STR)
						i = (strcmp( TS[ x._i ], TS[ y._i ] ) > 0);
					else throw ExecErr("*** EXECUTION ERROR: wrong types on operator > ***");
					if (i)
						args.push( arg( true, LEX_TRUE ) );
					else
						args.push( arg( false, LEX_FALSE ) );
				}
					break;
				case LEX_LEQ:
				{
					bool i;
					arg y = args.pop();
					arg x = args.pop();
					if (x._t == LEX_NUM && y._t == LEX_NUM)
						i = x._v.i <= y._v.i;
					else if (x._t == LEX_NUM && y._t == LEX_DOUBLE)
						i = x._v.i <= TR[ y._i ];
					else if (x._t == LEX_DOUBLE && y._t == LEX_NUM)
						i = TR[ x._i ] <= y._v.i;
					else if (x._t == LEX_DOUBLE && y._t == LEX_DOUBLE)
						i = TR[ x._i ] <= TR[ y._i ];
					else throw ExecErr("*** EXECUTION ERROR: wrong types on operator <= ***");
					if (i)
						args.push( arg( true, LEX_TRUE ) );
					else
						args.push( arg( false, LEX_FALSE ) );
				}
					break;
				case LEX_GEQ:
				{
					bool i;
					arg y = args.pop();
					arg x = args.pop();
					if (x._t == LEX_NUM && y._t == LEX_NUM)
						i = x._v.i >= y._v.i;
					else if (x._t == LEX_NUM && y._t == LEX_DOUBLE)
						i = x._v.i >= TR[ y._i ];
					else if (x._t == LEX_DOUBLE && y._t == LEX_NUM)
						i = TR[ x._i ] >= y._v.i;
					else if (x._t == LEX_DOUBLE && y._t == LEX_DOUBLE)
						i = TR[ x._i ] >= TR[ y._i ];
					else throw ExecErr("*** EXECUTION ERROR: wrong types on operator >= ***");
					if (i)
						args.push( arg( true, LEX_TRUE ) );
					else
						args.push( arg( false, LEX_FALSE ) );
				}
					break;
				case LEX_NEQ:
				{
					bool i;
					arg y = args.pop();
					arg x = args.pop();
					if (x._t == LEX_NUM && y._t == LEX_NUM)
						i = x._v.i != y._v.i;
					else if (x._t == LEX_NUM && y._t == LEX_DOUBLE)
						i = x._v.i != TR[ y._i ];
					else if (x._t == LEX_DOUBLE && y._t == LEX_NUM)
						i = TR[ x._i ] != y._v.i;
					else if (x._t == LEX_DOUBLE && y._t == LEX_DOUBLE)
						i = TR[ x._i ] != TR[ y._i ];
					else if (x._t == LEX_STR && y._t == LEX_STR)
						i = (strcmp( TS[ x._i ], TS[ y._i ] ) != 0);
					else throw ExecErr("*** EXECUTION ERROR: wrong types on operator != ***");
					if (i)
						args.push( arg( true, LEX_TRUE ) );
					else
						args.push( arg( false, LEX_FALSE ) );
				}
					break;
				case LEX_ASSIGN:
				{
					arg x = args.pop();
					arg y = args.pop();
					if ((x._t == LEX_NUM || x._t == LEX_TRUE || x._t == LEX_FALSE) && (LTID.get_typeid( TID[y._v.i].getname() ) == LEX_INT || LTID.get_typeid( TID[y._v.i].getname() ) == LEX_BOOL))
					{
						LTID.put_value( TID[y._v.i].getname(), x._v.i );
						args.push( arg( LTID.get_value( TID[y._v.i].getname() ), LTID.get_typeid( TID[y._v.i].getname() ) ) );
					}
					else if (x._t == POLIZ_ADDRESS && (LTID.get_typeid( TID[y._v.i].getname() ) == LEX_INT || LTID.get_typeid( TID[y._v.i].getname() ) == LEX_BOOL))
					{
						if (LTID.get_typeid( TID[x._v.i].getname() ) == LEX_BOOL || LTID.get_typeid( TID[x._v.i].getname() ) == LEX_INT)
							LTID.put_value( TID[y._v.i].getname(), LTID.get_value( TID[x._v.i].getname() ) );
						else if (LTID.get_typeid( TID[x._v.i].getname() ) == LEX_REAL)
							LTID.put_value( TID[y._v.i].getname(), TR[ LTID.get_value( TID[x._v.i].getname() ) ] );
						args.push( arg( LTID.get_value( TID[y._v.i].getname() ), LTID.get_typeid( TID[y._v.i].getname() ) ) );
					}
					else if (LTID.get_typeid( TID[y._v.i].getname() ) == LEX_REAL && (x._t == LEX_NUM || x._t == LEX_TRUE || x._t == LEX_FALSE))
					{
						if ( LTID.get_assign( TID[y._v.i].getname() ) )
						{
							TR[LTID.get_value( TID[y._v.i].getname() )] = x._v.i;
						}
						else
						{
							int i = TR.put(x._v.i);
							LTID.put_value( TID[y._v.i].getname(), i );
						}
						args.push( arg( LTID.get_value( TID[y._v.i].getname() ), LEX_DOUBLE ) );
					}
					else if (LTID.get_typeid( TID[y._v.i].getname() ) == LEX_REAL && x._t == POLIZ_ADDRESS)
					{
						if (LTID.get_typeid( TID[x._v.i].getname() ) == LEX_BOOL || LTID.get_typeid( TID[x._v.i].getname() ) == LEX_INT)
							if ( LTID.get_assign( TID[y._v.i].getname() ) )
							{
								TR[ LTID.get_value( TID[y._v.i].getname() ) ] = LTID.get_value( TID[x._v.i].getname() );
							}
							else
							{
								int i = TR.put(LTID.get_value( TID[x._v.i].getname() ));
								LTID.put_value( TID[y._v.i].getname(), i );
							}
						else if (LTID.get_typeid( TID[x._v.i].getname() ) == LEX_REAL)
							if ( LTID.get_assign( TID[y._v.i].getname() ) )
							{
								TR[ LTID.get_value( TID[y._v.i].getname() ) ] = TR[ LTID.get_value( TID[x._v.i].getname() ) ];;
							}
							else
							{
								int i = TR.put(TR[ LTID.get_value( TID[x._v.i].getname() ) ]);
								LTID.put_value( TID[y._v.i].getname(), i );
							}
						args.push( arg( LTID.get_value( TID[y._v.i].getname() ), LEX_DOUBLE ) );
					}
					else if ((LTID.get_typeid( TID[y._v.i].getname() ) == LEX_INT || LTID.get_typeid( TID[y._v.i].getname() ) == LEX_BOOL) && x._t == LEX_DOUBLE)
					{
						LTID.put_value( TID[y._v.i].getname(), TR[x._i] );
						args.push( arg( LTID.get_value( TID[y._v.i].getname() ), LTID.get_typeid( TID[y._v.i].getname() ) ) );
					}
					else if (x._t == LEX_DOUBLE && LTID.get_typeid( TID[y._v.i].getname() ) == LEX_REAL)
					{
						if ( LTID.get_assign( TID[y._v.i].getname() ) )
						{
							TR[ LTID.get_value( TID[y._v.i].getname() ) ] = TR[x._i];
						}
						else
						{
							int i = TR.put(TR[x._i]);
							LTID.put_value( TID[y._v.i].getname(), i );
						}
						args.push( arg( LTID.get_value( TID[y._v.i].getname() ), LEX_DOUBLE ) );
					}
					else if (x._t == LEX_STR && LTID.get_typeid( TID[y._v.i].getname() ) == LEX_STRING)
					{
						if ( LTID.get_assign( TID[y._v.i].getname() ) )
						{
							char* s = new char [strlen(TS[x._i]) + 1];
							strcpy( s, TS[ x._i] );
							delete TS[ LTID.get_value( TID[y._v.i].getname() ) ];
							TS[ LTID.get_value( TID[y._v.i].getname() ) ] = s;
						}
						else
						{
							int i = TS.put(TS[x._i]);
							LTID.put_value( TID[y._v.i].getname(), i );
						}
						args.push( arg( LTID.get_value( TID[y._v.i].getname() ), LEX_STR ) );
					}
					else if (x._t == POLIZ_ADDRESS && LTID.get_typeid( TID[y._v.i].getname() ) == LEX_STRING)
					{
						TS[ LTID.get_value( TID[y._v.i].getname() ) ] = new char [strlen(TS[ LTID.get_value( TID[x._v.i].getname() ) ]) + 1];
						if ( LTID.get_assign( TID[y._v.i].getname() ) )
						{
							delete TS[ LTID.get_value( TID[y._v.i].getname() ) ];
							TS[LTID.get_value( TID[y._v.i].getname() )] = new char [strlen(TS[LTID.get_value( TID[x._v.i].getname() )]) + 1];
							strcpy( TS[ LTID.get_value( TID[y._v.i].getname() ) ], TS[ LTID.get_value( TID[x._v.i].getname() ) ] );
						}
						else
						{
							int i = TS.put(TS[ LTID.get_value( TID[x._v.i].getname() ) ]);
							LTID.put_value( TID[y._v.i].getname(), i );
						}
						args.push( arg( LTID.get_value( TID[y._v.i].getname() ), LEX_STR ) );
					}
					//else if (x._t == LEX_STRUCT && y._t == LEX_STRUCT)
					else throw ExecErr("*** EXECUTION ERROR: wrong types on assign ***");
					LTID.put_assign( TID[y._v.i].getname() );
				}
					break;
				case LEX_SEMICOLON:
					while (args.isempty() == false)
						args.pop();
					break;
				default:
					throw ExecErr("*** EXECUTION ERROR: unexpected elem ***");
			}
		}
		cerr << endl << "\t\tExecute is finished success :)" << endl;
	}
};

class Interpretator
{
	Parser _pars;
	Executer _exec;
public:
	Interpretator(char* program): _pars(program) {}
	void interpretation()
	{
		_pars.analyze();
		cerr << endl << "\t\tExecution:" << endl << endl;
		_exec.execute(_pars._prog);
	}
	const int errstr() const
	{
		return _pars.errstr();
	}
	const char* lexstr(Lex l) const
	{
		return _pars.lexstr(l);
	}
};

int main(int argc, char** argv)
{
	try {	
		if (argc != 2) throw argc;
		Interpretator i(argv[1]);	
		try {
			i.interpretation();
			return 0;
		}
		catch (const signed char c)
		{ 
			if (c == EOF)
			{
				cerr << endl << endl << "\t\t*** LEXIS ERROR:" << i.errstr() << ": end of program('$') not found ***" << endl << endl;
			}
			else
			{
				cerr << endl << endl << "\t\t*** LEXIS ERROR:" << i.errstr() << ": '" << c << "' is a wrong symbol ***"<< endl << endl;
			}
			return 1;
		}
		catch (SemErr& err)
		{
			cerr << endl << "\t\t*** SEMANTICS ERROR:" << i.errstr() << ':';
			err.print();
			return 1;
		}
		catch (Error& err)
		{
			err.print();
			return 1;
		}
		catch (Lex& l)
		{
			const char* s = i.lexstr(l);
			if (strcmp(s,"") == 0)
			{
				if (l.gettype() == LEX_DOUBLE)
					cerr << endl << "\t\t*** SYNTAX ERROR:" << i.errstr() << ": '" << TR[l.getvalue()] << "' is a wrong lexeme ***" << endl << endl;
				else if (l.gettype() == LEX_NUM)
					cerr << endl << "\t\t*** SYNTAX ERROR:" << i.errstr() << ": '" << l.getvalue() << "' is a wrong lexeme ***" << endl << endl;
				else
					cerr << endl << "\t\t*** SYNTAX ERROR:" << i.errstr() << ": '" << l << "' is a wrong lexeme ***" << endl << endl;
			}
			else
			{
				cerr << endl << "\t\t*** SYNTAX ERROR:" << i.errstr() << ": '" << s << "' is a wrong lexeme ***" << endl << endl;
			}
			return 1;
		}
	}
	catch (int i)
	{
		cerr << endl << "\t\t*** ERROR: Wrong number of arguments: " << i << ", but expected 2 ***" << endl << endl;
		return 1;
	}
	catch (const char* s)
	{
		cerr << endl << "\t\t*** ERROR: File '" << s << "' not found ***" << endl << endl;
		return 1;
	}
}
