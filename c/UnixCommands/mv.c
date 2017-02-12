#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <dirent.h>

int main(int argc, char** argv)
{
	if (argc != 3)
	{
		printf("wrong number of arguments\n");
		return -1;
	}
	if (strcmp(argv[1],argv[2]) == 0)
		return 0;
	int from = open(argv[1],O_RDONLY,0);
	if (from == -1)
	{
		printf("first file not found\n");
		return -1;
	}
	struct stat* st;
	if (stat(argv[1],st) != 0)
	{
		printf("perms error");
		close(from);
		return -1;
	}
	int perms = 0777 & st->st_mode;
	int to = creat(argv[2],perms);
	if (to == -1)
	{
		DIR* dp = opendir(argv[2]);
		if (dp == 0)
		{
			printf("second file not found\n");
			close(from);
			return -1;
		}
		char* file = argv[2];
		file = strcat(file,"/");
		file = strcat(file,argv[1]);
		to = creat(file,perms);
	}
	int n;
	char buf[BUFSIZ];
	while ((n = read(from,buf,BUFSIZ)) > 0)
		if (write(to,buf,n) != n)
			printf("writing error");
	close(from);
	close(to);
	unlink(argv[1]);
	return 0;
}
