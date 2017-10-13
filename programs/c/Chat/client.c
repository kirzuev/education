#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

int main(int argc, char** argv)
{
	if (argc != 3)
	{
		fprintf(stderr, "\n***ERROR: wrong number of arguments***\n");
		return -1;
	}
	int fd;
	int port = atoi(argv[2]);
	struct sockaddr_in addr;
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	if (!inet_aton(argv[1], &(addr.sin_addr)))
	{
		fprintf(stderr, "\n***ERROR: wrong IP adress***\n");
		return -1;
	}
	if ((fd =  socket(AF_INET, SOCK_STREAM, 0)) == -1)
	{
		fprintf(stderr, "\n***ERROR: socket haven't been created***\n");
		return -1;
	}
	if (connect(fd, (struct sockaddr*) &addr, sizeof(addr)) == -1)
	{
		fprintf(stderr, "\n***ERROR: can't connect to server***\n");
		return -1;
	}
	char name[10];
	char buf[76];
	printf("\nInput your nickname please: ");
	if (scanf("%s", name) < 0)
	{
		fprintf(stderr, "\n***ERROR: wrong name!***\n");
		return -1;
	}
	int i;
	for (i = 0; i < 10; i++)
		if (name[i] == '\n' || i == 9)
		{
			name[i] = '\0';
			break;
		}
	send(fd, name, strlen(name)+1, 0);
	recv(fd, buf, 76, 0);
	printf("%s\n",buf);
	int pid;
	if ((pid = fork()))
	{
		close(1);
		while (scanf("%s",buf) > 0)
		{
			if (strcmp(buf, "exit") == 0)
			{
				kill(pid, SIGTERM);
				printf("\nDisconnected\n");
				return 0;
			}
			send(fd, buf, strlen(buf)+1, 0);
		}
		kill(pid, SIGTERM);
		printf("\nDisconnected\n");
	}
	else
	{
		close(0);
		while (recv(fd, buf, 76, 0))
		{
			printf("%s\n", buf);
		}
		kill(getppid(), SIGTERM);
		printf("\nDisconnected\n");
	}
	return 0;
}
