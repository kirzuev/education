#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char** argv)
{
	int i;
	int c;
	FILE* fp;
	if (argc < 2)
	{
		printf("error: less arguments\n");
		return -1;
	}
	if (strcmp(argv[1],"-n") == 0)
	{	
		int n = 1;
		if (argc < 3)
		{
			printf("error: less arguments\n");
			return -1;
		}
		for (i = 2; i < argc; i++)
		{
			fp = fopen(argv[i],"r");
			if (fp == NULL)
			{
				printf("file not found");
				return -1;
			}
			while ((c = getc(fp)) != EOF)
			{
				printf("%d %c",n++,c);
				if (c == '\n') continue;
				do
				{
					c = getc(fp);
					putchar(c);
				} while (c != '\n' && c != EOF);
			}
			fclose(fp);
		}
	}
	else for (i = 1; i < argc; i++)
		{
			fp = fopen(argv[i],"r");
			if (fp == NULL)
			{
				printf("file not found");
				return -1;
			}
			while ((c = getc(fp)) != EOF)
				putchar(c);
		}
	return 0;
}
