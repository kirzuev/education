%{
#include <stdio.h>
%}
%%
	P: S { printf("result: %d\n", $1); }
	S: S'+'A { $$ = $1+$3; } | S'-'A { $$ = $1-$3; } | A { $$ = $1; }
	A: A'*'B { $$ = $1*$3; } | A'/'B { $$ = $1/$3; } | B { $$ = $1; }
	B: N { $$ = $1; } | '('S')' { $$ = $2; }
	N: W N { $$ = $1*10 + $2; } | W { $$ = $1; } | '0' { $$ = 0; }
	W: '1' { $$ = 1; } | '2' { $$ = 2; } | '3' { $$ = 3; }
	| '4' { $$ = 4; } | '5' { $$ = 5; } | '6' { $$ = 6; }
	| '7' { $$ = 7; } | '8' { $$ = 8; } | '9' { $$ = 9; }
%%

main()
{
	printf ( "type an expression, please: " );
	yyparse ();
}

yylex()
{
	int c;
	c = getchar();
	if (c == '\n') return 0;
	yylval = c;
	return c;
};

yyerror(char *s)
{
	printf ("*** ERROR: %s ***\n", s);
};
