#include <stdio.h>

int main(int argc, char** argv)
{
	int i;
	int n = 1;
	if (argc != 1)
		if (strcmp(argv[1],"-n") == 0)
		{
			n = 0;
			for (i = 2; i < argc; i++)
				printf("%s ",argv[i]);
		}
		else for (i = 1; i < argc; i++)
			printf("%s ", argv[i]);
	if (n) putchar('\n');
	return 0;
}
