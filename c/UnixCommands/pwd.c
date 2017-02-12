#include <stdio.h>
#include <unistd.h>

int main(void)
{
	char s[256];
	printf("%s\n",getcwd(s,256));
	return 0;
}
