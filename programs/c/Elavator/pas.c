#include <stdio.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <unistd.h>

int main(int argc, char** argv)
{
	if (argc != 3)
	{
		fprintf(stderr, "\n*** wrong number of argumets! ***\n");
		return -1;
	}
	int sec = atoi(argv[1]);
	int weight = atoi(argv[2]);
	key_t k = ftok("lift",0);
	int s = semget(k,1,0);
	if (s == -1)
	{
		fprintf(stderr, "\n*** the elavator isn't working now! ***\n");
		return -1;
	}
	if (!fork())
	{
		if (!fork())
		{
			struct sembuf op = {0, - weight, 0};
			semop(s,&op,1);
			//printf("\n*** IN *** : PID = %d; current weight = %d\n", getpid(), 500 - semctl(s,0,GETVAL,(int) 0));
			sleep(sec);
			op.sem_op = weight;
			semop(s,&op,1);
			//printf("\n*** OUT *** : PID = %d; current weight = %d\n", getpid(), 500 - semctl(s,0,GETVAL,(int) 0));
			return 0;
		}
		exit(0);
	}
	wait(NULL);
	return 0;
}
