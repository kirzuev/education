#include <stdio.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <signal.h>
#include <stdlib.h>

int s;

void handler(int signo)
{
	semctl(s,0,IPC_RMID,(int) 0);
	printf("\n*** STOP! ***\n");
	exit(0);
	return;
}

int main(void)
{
	printf("\n*** START! ***\n");
	key_t k = ftok("lift",0);
	s = semget(k, 1, IPC_CREAT | 0666);
	semctl(s,0,SETVAL,(int) 500);
	signal(SIGINT, handler);
	while (1)
	{
		sleep(1);
		printf("\n*** the elavator is working now; current weight = %d ***\n", 500 - semctl(s,0,GETVAL,(int) 0));
	}
	return 0;
}
