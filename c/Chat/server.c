#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/time.h>

struct list
{
	int fd;
	char name[10];
	struct list* next;
};

struct list* clear_list(struct list* l)
{
	if (l == NULL) return NULL;
	if (l->next != NULL) clear_list(l->next);
	free(l);
	return NULL;
}

struct list* remove_fd(struct list* l, int fd)
{
	if (l == NULL) return NULL;
	if (l->fd == fd)
	{
		struct list* t = l;
		l = l->next;
		shutdown(t->fd,2);
		close(t->fd);
		free(t);
	}
	else
	{
		l->next = remove_fd(l->next, fd);
	}
	return l;
}

struct list *l = NULL;
int sd;

void handler(int s)
{
	l = clear_list(l);
	shutdown(sd,2);
	close(sd);
	fprintf(stderr,"\nServer is turned OFF\n");
	exit(0);
}

int main(int argc, char** argv)
{
	if (argc != 2)
	{
		fprintf(stderr, "\n***ERROR: wrong number of arguments***\n");
		return -1;
	}
	int port = atoi(argv[1]);
	int fd;
	char buf[64];
	struct sockaddr_in addr;
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = INADDR_ANY;

	signal(SIGINT, handler);
	if ((sd =  socket(AF_INET, SOCK_STREAM, 0)) == -1)
	{
		fprintf(stderr, "\n***ERROR: socket haven't been created***\n");
		return -1;
	}
	if (bind(sd, (struct sockaddr*) &addr, sizeof(addr)) == -1)
	{
		fprintf(stderr, "\n***ERROR: socket haven't been binded***\n");
		shutdown(sd,2);
		close(sd);
		return -1;
	}
	if (listen(sd, 5) == -1)
	{
		fprintf(stderr, "\n***ERROR: socket listening haven't been started***\n");
		shutdown(sd,2);
		close(sd);
		return -1;
	}
	l = (struct list*) malloc (sizeof(struct list));
	l->next = NULL;
	l->fd = accept(sd, NULL, NULL);
	recv(l->fd, l->name, 10, 0);
	printf("%s is joined\n", l->name);
	char mes[20];
	strcpy(mes, l->name);
	strcat(mes, " is joined\n");
	send(l->fd, mes, strlen(mes)+1, 0);
	while (1)
	{
		fd_set fds;
		int max = sd;
		struct list *p;
		int res;
		FD_ZERO(&fds);
		FD_SET(sd, &fds);
		for (p = l; p != NULL; p = p->next)
		{
			FD_SET(p->fd, &fds);
			if (p->fd > max) max = p->fd;
		}
		if ((res = select(max+1, &fds, NULL, NULL, NULL)) == -1)
		{
			fprintf(stderr, "\n***ERROR: select()***\n");
			p = clear_list(p);
			shutdown(sd,2);
			close(sd);
			return -1;
		}
		if (FD_ISSET(sd, &fds))
		{
			p = l;
			while (p->next != NULL) p = p->next;
			p->next = (struct list*) malloc(sizeof(struct list));
			p = p->next;
			p->fd = accept(sd, NULL, NULL);
			p->next = NULL;
			recv(p->fd, p->name, 10, 0);
			printf("%s is joined\n", p->name);
			struct list *t;
			for (t = l; t != NULL; t = t->next)
			{
				char mes[20];
				strcpy(mes, p->name);
				strcat(mes, " is joined\n");
				send(t->fd, mes, strlen(mes)+1, 0);
			}
		}
		for (p = l; p != NULL; p = p->next)
			if (FD_ISSET(p->fd, &fds))
			{
				if (recv(p->fd, buf, 64, 0))
				{
					struct list *t;
					int f;
					char mes[76];
					strcpy(mes, p->name);
					strcat(mes, ": ");
					strcat(mes, buf);
					for (t = l; t != NULL; t = t->next)
						send(t->fd, mes, strlen(mes)+1, 0);
				}
				else
				{
					char mes[20];
					strcpy(mes, p->name);
					strcat(mes, " has left\n");
					printf("%s", mes);
					l = remove_fd(l, p->fd);
					struct list *t;
					for (t = l; t != NULL; t = t->next)
						send(t->fd, mes, strlen(mes)+1, 0);
				}
			}
	}
}
