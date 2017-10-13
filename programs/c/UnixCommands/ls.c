#include <stdlib.h>
#include <pwd.h>
#include <grp.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <dirent.h>

void printdir(char* path, DIR* dp, int r, int l, int g)
{
	struct dirent* dir;
	struct stat st;
	char* name;
	while (dir = readdir(dp))
		if (dir->d_name[0] != '.')
		{	
			if (r || l || g)
			{
				name = (char*) malloc (strlen(path) + strlen(dir->d_name) + 2);
				name = strcpy(name,path);
				name = strcat(name,"/");
				name = strcat(name,dir->d_name);
				if (stat(name,&st) != 0)
				{
					printf("perms error\n");
					exit(1);
				}
				free(name);
				if (l)
				{
					switch (st.st_mode & S_IFMT)
					{
						case S_IFDIR: putchar('d'); break;
						case S_IFCHR: putchar('c'); break;
						case S_IFBLK: putchar('b'); break;
						case S_IFREG: putchar('-'); break;
						case S_IFLNK: putchar('l'); break;
						case S_IFSOCK: putchar('s'); break;
					}
					static const char dflt[10] = "rwxrwxrwx";
					char perms[10];
					int i,j;
					struct passwd *pw_d;
					for (i = 0, j = (1 << 8); i < 9; i++, j >>= 1)
						perms[i] = (st.st_mode & j) ? dflt[i] : '-';
					if (st.st_mode & S_ISUID) perms[2] = 's';
					if (st.st_mode & S_ISGID) perms[5] = 's';
					if (st.st_mode & S_ISVTX) perms[8] = 't';
					perms[9] = '\0';
					pw_d = getpwuid(st.st_uid);
					printf("%s %10s %10d\t", perms, pw_d->pw_name, st.st_size);
				}
				if (g)
				{
					struct group *gr_d;
					gr_d = getgrgid(st.st_gid);
					printf("%10s\t", gr_d->gr_name);
				}
			}
				printf("%s\n", dir->d_name);
				if (r && (st.st_mode & S_IFDIR))
				{
					name = (char*) malloc (strlen(path) + strlen(dir->d_name) + 2);
					name = strcpy(name,path);
					name = strcat(name,dir->d_name);
					name = strcat(name,"/");
					printf("%s:\n",name);
					DIR* subdp = opendir(name);
					printdir(name,subdp,r,l,g);
					free(name);
					putchar('\n');
				}
		}
	closedir(dp);
	return;
}

int main(int argc, char** argv)
{
	char* path = "./";
	DIR* dp = opendir(".");
	switch (argc)
	{
		case 1: printdir(path,dp,0,0,0); break;
		case 2: if (strcmp(argv[1],"-R") == 0)
			{
				printdir(path,dp,1,0,0);
				break;
			}
			if (strcmp(argv[1],"-l") == 0)
			{
				printdir(path,dp,0,1,0);
				break;
			}
			if (strcmp(argv[1],"-g") == 0)
			{
				printdir(path,dp,0,0,1);
				break;
			}
			closedir(dp);
			if (dp = opendir(argv[1]))
			{
				path = (char*) malloc (strlen(argv[1]) + 2);
				path = strcpy(path,argv[1]);
				path = strcat(path,"/");
				printdir(path,dp,0,0,0);
				break;
			}
			else
			{
				printf("dir not found\n");
				return -1;
			}
			printf("wrong arguments\n");
			return -1;
		case 3: if (strcmp(argv[1],"-R") == 0)
			{	
				if (strcmp(argv[2],"-l") == 0)
				{
					printdir(path,dp,1,1,0);
					break;
				}
				if (strcmp(argv[2],"-g") == 0)
				{
					printdir(path,dp,1,0,1);
					break;
				}
				closedir(dp);
				if (dp = opendir(argv[2]))
				{
					path = (char*) malloc (strlen(argv[2]) + 2);
					path = strcpy(path,argv[2]);
					path = strcat(path,"/");
					printdir(path,dp,1,0,0);
					break;
				}
				else
				{
					printf("dir not found\n");
					return -1;
				}
			}
			if (strcmp(argv[1],"-l") == 0)
			{
				if (strcmp(argv[2],"-R") == 0)
				{
					printdir(path,dp,1,1,0);
					break;
				}
				if (strcmp(argv[2],"-g") == 0)
				{
					printdir(path,dp,0,1,1);
					break;
				}
				closedir(dp);
				if (dp = opendir(argv[2]))
				{	
					path = (char*) malloc (strlen(argv[2]) + 2);
					path = strcpy(path,argv[2]);
					path = strcat(path,"/");
					printdir(path,dp,0,1,0);
					break;
				}
				else 
				{
					printf("dir not found\n");
					return -1;
				}
			}
			if (strcmp(argv[1],"-g") == 0);
			{
				if (strcmp(argv[2],"-R") == 0)
				{
					printdir(path,dp,1,0,1);
					break;
				}
				if (strcmp(argv[2],"-l") == 0)
				{
					printdir(path,dp,0,1,1);
					break;
				}
				closedir(dp);
				if (dp = opendir(argv[2]))
				{
					path = (char*) malloc (strlen(argv[2]) + 2);
					path = strcpy(path,argv[2]);
					path = strcat(path,"/");
					printdir(path,dp,0,0,1);
					break;
				}
				else
				{
					printf("dir not found\n");
					return -1;
				}
			}
			printf("wrong arguments\n");
			return -1;
		case 4: if (strcmp(argv[1],"-R") == 0)
			{
				if (strcmp(argv[2],"-l") == 0)
				{
					if (strcmp(argv[3],"-g") == 0)
					{
						printdir(path,dp,1,1,1);
						break;
					}
					closedir(dp);
					if (dp = opendir(argv[3]))
					{
						path = (char*) malloc (strlen(argv[3]) + 2);
						path = strcpy(path,argv[3]);
						path = strcat(path,"/");
						printdir(path,dp,1,1,0);
						break;
					}
					else
					{
						printf("dir not found\n");
						return -1;
					}
				}
				if (strcmp(argv[2],"-g") == 0)
				{
					if (strcmp(argv[3],"-l") == 0)
					{
						printdir(path,dp,1,1,1);
						break;
					}
					closedir(dp);
					if (dp = opendir(argv[3]))
					{
						path = (char*) malloc (strlen(argv[3]) + 2);
						path = strcpy(path,argv[3]);
						path = strcat(path,"/");
						printdir(path,dp,1,0,1);
						break;
					}
					else
					{
						printf("dir not found\n");
						return -1;
					}
				}
			}
			if (strcmp(argv[1],"-l") == 0)
			{
				if (strcmp(argv[2],"-R") == 0)
				{
					if (strcmp(argv[3],"-g") == 0)
					{
						printdir(path,dp,1,1,1);
						break;
					}
					closedir(dp);
					if (dp = opendir(argv[3]))
					{
						path = (char*) malloc (strlen(argv[3]) + 2);
						path = strcpy(path,argv[3]);
						path = strcat(path,"/");
						printdir(path,dp,1,1,0);
						break;
					}
					else
					{
						printf("dir not found\n");
						return -1;
					}
				}
				if (strcmp(argv[2],"-g") == 0)
				{
					if (strcmp(argv[3],"-R") == 0)
					{
						printdir(path,dp,1,1,1);
						break;
					}
					closedir(dp);
					if (dp = opendir(argv[3]))
					{
						path = (char*) malloc (strlen(argv[3]) + 2);
						path = strcpy(path,argv[3]);
						path = strcat(path,"/");
						printdir(path,dp,0,1,1);
						break;
					}
					else
					{
						printf("dir not found\n");
						return -1;
					}
				}
			}
			if (strcmp(argv[1],"-g") == 0)
			{
				if (strcmp(argv[2],"-R") == 0)
				{
					if (strcmp(argv[3],"-l") == 0)
					{
						printdir(path,dp,1,1,1);
						break;
					}
					closedir(dp);
					if (dp = opendir(argv[3]))
					{
						path = (char*) malloc (strlen(argv[3]) + 2);
						path = strcpy(path,argv[3]);
						path = strcat(path,"/");
						printdir(path,dp,1,0,1);	
						break;
					}
					else
					{
						printf("dir not found\n");
						return -1;
					}
				}
				if (strcmp(argv[2],"-l") == 0)
				{
					if (strcmp(argv[3],"-R") == 0)
					{
						printdir(path,dp,1,1,1);
						break;
					}
					closedir(dp);
					if (dp = opendir(argv[3]))
					{
						path = (char*) malloc (strlen(argv[3]) + 2);
						path = strcpy(path,argv[3]);
						path = strcat(path,"/");
						printdir(path,dp,0,1,1);
						break;
					}
					else
					{
						printf("dir not found\n");
						return -1;
					}
				}
			}
			printf("wrong arguments\n");
			return -1;
		case 5: if (strcmp(argv[1],"-R") == 0)
			{
				if (strcmp(argv[2],"-l") == 0)
				{
					if (strcmp(argv[3],"-g") == 0)
					{
						closedir(dp);
						if (dp = opendir(argv[4]))
						{
							path = (char*) malloc (strlen(argv[4]) + 2);
							path = strcpy(path,argv[4]);
							path = strcat(path,"/");
							printdir(path,dp,1,1,1);
							break;
						}
						else
						{
							printf("dir not found\n");
							return -1;
						}
					}
					printf("wrong arguments\n");
					return -1;
				}
				if (strcmp(argv[2],"-g") == 0)
				{
					if (strcmp(argv[3],"-l") == 0)
					{
						closedir(dp);
						if (dp = opendir(argv[4]))
						{
							path = (char*) malloc (strlen(argv[4]) + 2);
							path = strcpy(path,argv[4]);
							path = strcat(path,"/");
							printdir(path,dp,1,1,1);
							break;
						}
						else 
						{
							printf("dir not found\n");
							return -1;
						}
					}
					printf("wrong arguments\n");
					return -1;
				}
			}
			if (strcmp(argv[1],"-l") == 0)
			{
				if (strcmp(argv[2],"-R") == 0)
				{
					if (strcmp(argv[3],"-g") == 0)
					{
						closedir(dp);
						if (dp = opendir(argv[4]))
						{
							path = (char*) malloc (strlen(argv[4]) + 2);
							path = strcpy(path,argv[4]);
							path = strcat(path,"/");
							printdir(path,dp,1,1,1);
							break;
						}
						else
						{
							printf("dir not found\n");
							return -1;
						}
					}
					printf("wrong arguments\n");
					return -1;
				}
				if (strcmp(argv[2],"-g") == 0)
				{
					if (strcmp(argv[3],"-R") == 0)
					{
						closedir(dp);
						if (dp = opendir(argv[4]))
						{
							path = (char*) malloc (strlen(argv[4]) + 2);
							path = strcpy(path,argv[4]);
							path = strcat(path,"/");
							printdir(path,dp,1,1,1);
							break;
						}
						else 
						{
							printf("dir not found\n");
							return -1;
						}
					}
					printf("wrong arguments\n");
					return -1;
				}
			}
			if (strcmp(argv[1],"-g") == 0)
			{
				if (strcmp(argv[2],"-l") == 0)
				{
					if (strcmp(argv[3],"-R") == 0)
					{
						closedir(dp);
						if (dp = opendir(argv[4]))
						{
							path = (char*) malloc (strlen(argv[4]) + 2);
							path = strcpy(path,argv[4]);
							path = strcat(path,"/");
							printdir(path,dp,1,1,1);
							break;
						}
						else
						{
							printf("dir not found\n");
							return -1;
						}
					}
					printf("wrong arguments\n");
					return -1;
				}
				if (strcmp(argv[2],"-R") == 0)
				{
					if (strcmp(argv[3],"-l") == 0)
					{
						closedir(dp);
						if (dp = opendir(argv[4]))
						{
							path = (char*) malloc (strlen(argv[4]) + 2);
							path = strcpy(path,argv[4]);
							path = strcat(path,"/");
							printdir(path,dp,1,1,1);
							break;
						}
						else 
						{
							printf("dir not found\n");
							return -1;
						}
					}
					printf("wrong arguments\n");
					return -1;
				}
			}
		default: printf("wrong number of arguments\n");
			 return -1;
	}
	return 0;
}
