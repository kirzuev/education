%{
#include <stdio.h>
%}
%%
	P: S { printf("pr: %d\n", $1); }
	S: '('S')'S { $$=$4+1; }| /*empty*/{ $$=0; }
%%

main()
{
	printf ( "type a string, please: " );
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
	printf ( "***ERROR: %s***\n", s );
};
