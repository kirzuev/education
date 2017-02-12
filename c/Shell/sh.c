#include <fcntl.h>
#include <pwd.h>
#include <sys/wait.h>
#include <unistd.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define N 20

typedef struct elem
{
	char* word;
	struct elem* next;
} list;

typedef struct cmd_inf
{
	char** argv;
	char* infile;
	char* outfile;
	int backgrnd;
	int append;
	struct cmd_inf* psubcmd;
	struct cmd_inf* pipe;
 	struct cmd_inf* next;
} tree;

int c, err, sub;

list* cmdlst(list*, tree*);
list* conv(list*, tree*);
list* cmd(list*, tree*);
list* simple(list*, tree*);

void print_tree(tree* t)
{
	int k,f;
	f = open("struct.txt",O_WRONLY|O_CREAT|O_APPEND,0666);
	if (!fork())
	{
		dup2(f,1);
		while (t != NULL)
		{
			k = 0;
			if (t->argv != NULL)
			{
				printf("\ncommand %s:\n\targv:\n", *(t->argv));
				while (t->argv[k] != NULL)
				{
					printf("\t\t%s\n", t->argv[k]);
					k++;
				}
			}
			if (t->psubcmd != NULL)
			{
				printf("\n/// subshell started \\\\\\\n");
				print_tree(t->psubcmd);
				printf("\n\\\\\\  subshell ends  ///\n");
			}
			printf("\n\tbackground:    %d\n", t->backgrnd);
			if (t->infile != NULL) printf("\tinfile:        %s\n", t->infile);
			if (t->outfile != NULL) printf("\toutfile:       %s\n", t->outfile);
			if (t->outfile != NULL) printf("\tappend:        %d\n", t->append);
			if (t->pipe != NULL)
			{
				printf("\n---------|pipe|---------\n");
				print_tree(t->pipe);
			}
			printf("\n-------------------------\n");
			t = t->next;
		}
		exit(0);
	}
	wait(NULL);
	return;
}

void execute(tree* t)
{
	if (t == NULL) return;
	if (t->argv != NULL)
	{
		if (t->pipe == NULL && strcmp(t->argv[0],"exit") == 0 && !t->backgrnd) exit(0);
		else if (t->pipe == NULL && strcmp(t->argv[0],"pwd") == 0)
		{
			int size = 64;
			char* path = (char*) malloc (size);
			while (getcwd(path,size) == NULL)
			{
				size *= 2;
				path = (char*) realloc (path,size);
			}
			path = (char*) realloc (path, strlen(path) + 1);
			if (t->outfile)
			{
				int fd,f0,fl = O_WRONLY|O_CREAT|O_TRUNC;
				if (t->append) fl = O_WRONLY|O_CREAT|O_APPEND;
				fd = open(t->outfile,fl,0666);
				f0 = dup(1);
				dup2(fd,1);
				close(fd);
				printf("%s\n",path);
				dup2(f0,1);
			}
			else printf("%s\n",path);
		}
		else if (t->pipe == NULL && strcmp(t->argv[0],"cd") == 0 )
		{
			if (t->argv[1] == NULL) chdir(getenv("HOME"));
			else if (chdir(t->argv[1]) == -1) fprintf(stderr,"\n***error***: dir '%s' not found\n",t->argv[1]);
		}
	}
	if (t->backgrnd)
	{
		if (!fork())
		{
			signal(SIGINT,SIG_IGN);
			dup2(open("dev/null",O_RDONLY),0);
			if (t->pipe || t->psubcmd && t->psubcmd->pipe)
			{
				int in, out, next_in, fd[2];
				pipe(fd);
				out = fd[1];
				next_in = fd[0];
				if (!fork())
				{
					close(next_in);
					dup2(out,1);
					if (t->infile)
					{
						int fd;
						if ((fd = open(t->infile,O_RDONLY)) == -1)
						{
							fprintf(stderr,"\n***error***: file '%s' not found\n",t->infile);
							exit(0);
						}
						dup2(fd,0);
						close(fd);
					}
					if (t->outfile)
					{
						int fd;
						if (t->append) fd = open(t->outfile,O_WRONLY|O_CREAT|O_APPEND,0666);
						else fd = open(t->outfile,O_WRONLY|O_CREAT|O_TRUNC,0666);
						dup2(fd,1);
						close(fd);
					}
					close(out);
					if (t->psubcmd) execute(t->psubcmd);
					else if (strcmp(t->argv[0],"exit") != 0 && strcmp(t->argv[0],"cd") != 0)
					{
						execvp(t->argv[0],t->argv);
						fprintf(stderr,"\n***wrong command '%s'***\n",t->argv[0]);
					}
					exit(0);
				}
				in = next_in;
				if (t->pipe) t = t->pipe;
				else t = t->psubcmd;
				while (t->pipe || t->psubcmd && t->psubcmd->pipe)
				{
					close(out);
					pipe(fd);
					out = fd[1];
					next_in = fd[0];
					if (!fork())
					{
						close(next_in);
						dup2(in,0);
						close(in);
						dup2(out,1);
						close(out);
						if (t->infile)
						{
							int fd;
							if ((fd = open(t->infile,O_RDONLY)) == -1)
							{
								fprintf(stderr,"\n***error***: file '%s' not found\n",t->infile);
								exit(0);
							}
							dup2(fd,0);
							close(fd);
						}
						if (t->outfile)
						{
							int fd;
							if (t->append) fd = open(t->outfile,O_WRONLY|O_CREAT|O_APPEND,0666);
							else fd = open(t->outfile,O_WRONLY|O_CREAT|O_TRUNC,0666);
							dup2(fd,1);
							close(fd);
						}
						if (t->psubcmd) execute(t->psubcmd);
						else if (strcmp(t->argv[0],"exit") != 0 && strcmp(t->argv[0],"cd") != 0)
						{
							execvp(t->argv[0],t->argv);
							fprintf(stderr,"\n***wrong command '%s'***\n",t->argv[0]);
						}
						exit(0);
					}	
					close(in);
					in = next_in;
					if (t->pipe) t = t->pipe;
					else t = t->psubcmd;
				}
				close(out);
				if (!fork())
				{
					dup2(in,0);
					close(in);
					if (t->infile)
					{
						int fd;
						if ((fd = open(t->infile,O_RDONLY)) == -1)
						{
							fprintf(stderr,"\n***error***: file '%s' not found\n",t->infile);
							exit(0);
						}
						dup2(fd,0);
						close(fd);
					}
					if (t->outfile)
					{
						int fd;
						if (t->append) fd = open(t->outfile,O_WRONLY|O_CREAT|O_APPEND,0666);
						else fd = open(t->outfile,O_WRONLY|O_CREAT|O_TRUNC,0666);
						dup2(fd,1);
						close(fd);
					}
					if (t->psubcmd) execute(t->psubcmd);
					else if (strcmp(t->argv[0],"exit") && strcmp(t->argv[0],"cd") != 0)
					{
						execvp(t->argv[0],t->argv);
						fprintf(stderr,"\n***wrong command '%s'***\n",t->argv[0]);
					}
					exit(0);
				}
				close(in);
			}
			else
			{
				if (!fork())
				{
					if (t->infile)
					{
						int fd;
						if ((fd = open(t->infile,O_RDONLY)) == -1)
						{
							fprintf(stderr,"\n***error***: file '%s' not found\n",t->infile);
							exit(0);
						}
						dup2(fd,0);
						close(fd);
					}
					if (t->outfile)
					{
						int fd;
						if (t->append) fd = open(t->outfile,O_WRONLY|O_CREAT|O_APPEND,0666);
						else fd = open(t->outfile,O_WRONLY|O_CREAT|O_TRUNC,0666);
						dup2(fd,1);
						close(fd);
					}
					if (t->psubcmd) execute(t->psubcmd);
					else if (strcmp(t->argv[0],"exit") != 0 && strcmp(t->argv[0],"cd") != 0 && strcmp(t->argv[0],"pwd") != 0)
					{
						execvp(t->argv[0],t->argv);
						fprintf(stderr,"\n***wrong command '%s'***\n",t->argv[0]);
					}
					exit(0);
				}
			}
			exit(0);
		}
		wait(NULL);
	}
	else
	{
		if (t->pipe != NULL)
		{
			int i, k = 1, in, out, next_in, fd[2];
			tree* p = t;
			pipe(fd);
			out = fd[1];
			next_in = fd[0];
			if (!p->psubcmd && (strcmp(p->argv[0],"exit") == 0 || strcmp(p->argv[0],"cd") == 0)) k--;
			else if (!fork())
			{
				close(next_in);
				dup2(out,1);
				if (p->infile)
				{
					int fd;
					if ((fd = open(p->infile,O_RDONLY)) == -1)
					{
						fprintf(stderr,"\n***error***: file '%s' not found\n",p->infile);
						exit(0);
					}
					dup2(fd,0);
					close(fd);
				}
				if (p->outfile)
				{
					int fd;
					if (p->append) fd = open(p->outfile,O_WRONLY|O_CREAT|O_APPEND,0666);
					else fd = open(p->outfile,O_WRONLY|O_CREAT|O_TRUNC,0666);
					dup2(fd,1);
					close(fd);
				}
				close(out);
				if (p->psubcmd) execute(p->psubcmd);
				else  //if (strcmp(p->argv[0],"exit") != 0)
				{
					execvp(p->argv[0],p->argv);
					fprintf(stderr,"\n***wrong command '%s'***\n",p->argv[0]);
				}
				exit(0);
			}
			in = next_in;
			p = p->pipe;
			while (p->pipe != NULL)
			{
				k++;
				close(out);
				pipe(fd);
				out = fd[1];
				next_in = fd[0];
				if (!p->psubcmd && (strcmp(p->argv[0],"exit") == 0 || strcmp(p->argv[0],"cd") == 0)) k--;
				else if (!fork())
				{
					close(next_in);
					dup2(in,0);
					close(in);
					dup2(out,1);
					close(out);
					if (p->infile)
					{
						int fd;
						if ((fd = open(p->infile,O_RDONLY)) == -1)
						{
							fprintf(stderr,"\n***error***: file '%s' not found\n",p->infile);
							exit(0);
						}
						dup2(fd,0);
						close(fd);
					}
					if (p->outfile)
					{
						int fd;
						if (p->append) fd = open(p->outfile,O_WRONLY|O_CREAT|O_APPEND,0666);
						else fd = open(p->outfile,O_WRONLY|O_CREAT|O_TRUNC,0666);
						dup2(fd,1);
						close(fd);
					}
					if (p->psubcmd) execute(p->psubcmd);
					else// if (strcmp(p->argv[0],"exit") != 0)
					{
						execvp(p->argv[0],p->argv);
						fprintf(stderr,"\n***wrong command '%s'***\n",p->argv[0]);
					}
					exit(0);
				}	
				close(in);
				in = next_in;
				p = p->pipe;
			}
			close(out);
			k++;
			if (!p->psubcmd && (strcmp(p->argv[0],"exit") == 0  || strcmp(p->argv[0],"cd") == 0)) k--;
			else if (!fork())
			{
				dup2(in,0);
				close(in);
				if (p->infile)
				{
					int fd;
					if ((fd = open(p->infile,O_RDONLY)) == -1)
					{
						fprintf(stderr,"\n***error***: file '%s' not found\n",p->infile);
						exit(0);
					}
					dup2(fd,0);
					close(fd);
				}
				if (p->outfile)
				{
					int fd;
					if (p->append) fd = open(p->outfile,O_WRONLY|O_CREAT|O_APPEND,0666);
					else fd = open(p->outfile,O_WRONLY|O_CREAT|O_TRUNC,0666);
					dup2(fd,1);
					close(fd);
				}
				if (p->psubcmd) execute(p->psubcmd);
				else// if (strcmp(p->argv[0],"exit") != 0)
				{
					execvp(p->argv[0],p->argv);
					fprintf(stderr,"\n***wrong command '%s'***\n",p->argv[0]);
				}
				exit(0);
			}
			close(in);
			for (i = 0; i<k; i++) wait(NULL);
		}
		else
		{
			if (!fork())
			{
				if (t->infile)
				{
					int fd;
					if ((fd = open(t->infile,O_RDONLY)) == -1)
					{
						fprintf(stderr,"\n***error***: file '%s' not found\n",t->infile);
						exit(0);
					}
					dup2(fd,0);
					close(fd);
				}
				if (t->outfile)
				{
					int fd;
					if (t->append) fd = open(t->outfile,O_WRONLY|O_CREAT|O_APPEND,0666);
					else fd = open(t->outfile,O_WRONLY|O_CREAT|O_TRUNC,0666);
					dup2(fd,1);
					close(fd);
				}
				if (t->psubcmd) execute(t->psubcmd);
				else if (strcmp(t->argv[0],"exit") != 0 && strcmp(t->argv[0],"cd") != 0 && strcmp(t->argv[0],"pwd") != 0)
				{
					execvp(t->argv[0],t->argv);
					fprintf(stderr,"\n***wrong command '%s'***\n",t->argv[0]);
				}
				exit(0);
			}
			wait(NULL);
		}
	}
	execute(t->next);
	return;
}

tree* new_node(void)
{
	tree* t = (tree*) malloc (sizeof(tree));
	t->argv = NULL;
	t->infile = NULL;
	t->outfile = NULL;
	t->backgrnd = 0;
	t->append = 0;
	t->psubcmd = NULL;
	t->pipe = NULL;
	t->next = NULL;
	return t;
}

tree* clear_tree(tree* t)
{
	if (t == NULL) return NULL;
	t->psubcmd = clear_tree(t->psubcmd);
	t->pipe = clear_tree(t->pipe);
	t->next = clear_tree(t->next);
	if (t->argv != NULL) free(t->argv);
	free(t);
	return NULL;
}

tree* build_tree(list* l)
{
	sub = 0;
	if (l == NULL) return NULL;
	tree* t = new_node();
	cmdlst(l,t);
	return t;
}

list* cmdlst(list* l, tree* t)
{
	if (l == NULL) return NULL;
	l = conv(l,t);
	if (err) return l;
	if (l == NULL) return NULL;
	do
	{
		switch (l->word[0])
		{
			case '|':
			case '>': 
			case ')': if (sub) return l;
			case '<':
			case '(':
				if (t->backgrnd)
				{
					while (t->next != NULL) t = t->next;
					t->next = new_node();
					l = conv(l,t->next);
					if (err) return l;
				}
				else
				{
					fprintf(stderr,"\n***syntax error***: ';' or '&' was expected\n");
					err = 1;
					return l;
				}
				break;
			case ';':
				if (l->next == NULL) return l;
				l = l->next;
				while (t->next != NULL) t = t->next;
				t->next = new_node();
				l = conv(l,t->next);
				if (err) return l;
				break;
			case '&':
				while (t->next != NULL) t = t->next;
				t->backgrnd = 1;
				l = l->next;
				break;
			default:
				if (!isalpha(l->word[0]))
				{
					fprintf(stderr,"\n***syntax error***: wrong symbol\n");
					err = 1;
					return l;
				}
				t->next = new_node();
				l = conv(l,t->next);
				break;
		}
	} while (l != NULL);
	return l;	
}

list* conv(list* l, tree* t)
{
	l = cmd(l,t);
	if (err) return l;
	if (l == NULL) return NULL;
	while (l->word[0] == '|')
	{
		if (l->next == NULL)
		{
			fprintf(stderr, "\n***syntax error***: after '|' expected the command\n");
			err = 1;
			return l;
		}
		l = l->next;
		while (t->pipe != NULL) t = t->pipe;
		t->pipe = new_node();
		l = cmd(l,t->pipe);
		if (err) return l;
		if (l == NULL) return NULL;
	}
	return l;
}

list* cmd(list* l, tree* t)
{
	switch (l->word[0])
	{
		case '|':
		case '&': 
		case ')':
		case '<':
		case ';':
		case '>': fprintf(stderr, "\n***syntax error***: the command or the group of commands was expected\n"); err = 1; return l; break; 
		case '(':
			if (l->next == NULL)
			{
				fprintf(stderr, "\n***syntax error***: the command or the group of commands was expected\n");
				err = 1;
				return l;
			}
			sub++;
			l = l->next;
			t->psubcmd = new_node();
			l = cmdlst(l,t->psubcmd);
			sub--;
			if (err) return l;
			if (l == NULL || l->word[0] != ')')
			{
				fprintf(stderr, "\n***syntax error***: ')' was expected\n");
				err = 1;
				return l;
			}
			l = l->next;
			break;
		default: l = simple(l,t);
			 if (err) return l; break;
	}
	if (l == NULL) return NULL;
		switch (l->word[0])
		{
			case '<':
				if (l->next == NULL)
				{
					fprintf(stderr, "\n***syntax error***: file was expected\n");
					err = 1;
					return l;
				}
				l = l->next;
				if (!isalpha(l->word[0]) && l->word[0] != '/')
				{
					fprintf(stderr, "\n***syntax error***: file was expected\n");
					err = 1;
					return l;
				}
				t->infile = l->word;
				if (l->next != NULL)
				{
					l = l->next;
					if (l->word[0] == '>')
					{
						if (strlen(l->word) == 2) t->append = 1;
						if (l->next == NULL)
						{
							fprintf(stderr, "\n***syntax error***: file was expected\n");
							err = 1;
							return l;
						}
						l = l->next;
						if (!isalpha(l->word[0]) && l->word[0] != '/')
						{
							fprintf(stderr, "\n***syntax error***: file was expected\n");
							err = 1;
							return l;
						}
						t->outfile = l->word;
						l = l->next;
					}
					else if (l->word[0] == '<')
						{
							fprintf(stderr,"\n***syntax error***: file was expected\n");
							err = 1;
							return l;
						}
				}
				else return NULL;
				break;		
			case '>':
				if (strlen(l->word) == 2) t->append = 1;
				if (l->next == NULL)
				{
					fprintf(stderr, "\n***syntax error***: file was expected\n");
					err = 1;
					return l;
				}
				l = l->next;
				if (!isalpha(l->word[0]) && l->word[0] != '/')
				{
					fprintf(stderr, "\n***syntax error***: file was expected\n");
					err = 1;
					return l;
				}
				t->outfile = l->word;
				if (l->next != NULL)
				{
					l = l->next;
					if (l->word[0] == '<')
					{
						if (l->next == NULL)
						{
							fprintf(stderr, "\n***syntax error***: file was expected\n");
							err = 1;
							return l;
						}
						l = l->next;
						if (!isalpha(l->word[0]) && l->word[0] != '/')
						{
							fprintf(stderr, "\n***syntax error***: file was expected\n");
							err = 1;
							return l;
						}
						t->infile = l->word;
						l = l->next;
					}
					else if (l->word[0] == '>')
						{
							fprintf(stderr,"\n***syntax error***: file was expected\n");
							err = 1;
							return l;
						}
				}
				else return NULL;
				break;
		}
	return l;
}

list* simple(list* l, tree* t)
{
	int k = 1;
	if (!isalpha(l->word[0]) && l->word[0] != '.')
	{
		fprintf(stderr, "\n***syntax error***: the command was expected\n");
		err = 1;
		return l;
	}
	t->argv = (char**) malloc (2*sizeof(char*));
	t->argv[0] = l->word;
	if (l->next == NULL)
	{
		t->argv[1] = NULL;
		return NULL;
	}
	l = l->next;
	while (l->word[0] != ')' && l->word[0] != '(' && l->word[0] != ';' && l->word[0] != '&' && l->word[0] != '|' && l->word[0] != '>' && l->word[0] != '<')
	{
		if (l->word[0] == '$')
			if (strcmp(l->word,"$HOME") == 0)
			{
				free(l->word);
				l->word = (char*) malloc (strlen(getenv("HOME")) + 1);
				strcpy(l->word,getenv("HOME"));
			}
			else if (strcmp(l->word,"$SHELL") == 0)
				{
					free(l->word);
					l->word = (char*) malloc (strlen(getenv("SHELL")) + 1);
					strcpy(l->word,getenv("SHELL"));
				}
				else if (strcmp(l->word,"$USER") == 0)
					{
						free(l->word);
						l->word = (char*) malloc (strlen(getlogin()) + 1);
						strcpy(l->word,getlogin());
					}
					else if (strcmp(l->word,"$EUID") == 0)
						{
							free(l->word);
							char* name = getpwuid(geteuid())->pw_name;
							l->word = (char*) malloc (strlen(name) + 1);
							strcpy(l->word,name);
						}
		t->argv = (char**) realloc (t->argv, (k+2)*sizeof(char*));
		t->argv[k] = l->word;
		k++;
		if (l->next == NULL)
		{
			t->argv[k] = NULL;
			return NULL;
		}
		l = l->next;
	}
	t->argv[k] = NULL;
	return l;
}
		

list* add_elem(list* p, char* w)
{
	if (p == NULL)
	{
		p = (list*) malloc (sizeof(list));
		p->word = w;
	}
	else p->next = add_elem(p->next,w);
	return p;
}

list* clear_list(list* p)
{
	if (p == NULL) return p;
	p->next = clear_list(p->next);
	free(p->word);
	free(p);
	return NULL;
}

void print_list(list* p)
{
	while (p != NULL)
	{
		printf("\n%s",p->word);
		p = p->next;
	}
	putchar('\n');
	return;
}

char* new_word(void)
{
	char* s = NULL;
	s = (char*) malloc (N+2);
	return s;
}

list* build_list(void)
{
	int k = 0;
	char* str = new_word();
	int length = N;
	list* lst = NULL;
	enum states {s1,s2,s3,s4,s5,s6};
	enum states st = s1;
	printf("myshell :) $ ");
	fflush(stdout);
	do
	{
		if ((c = getchar()) != ' ' && c != '\t' && c != '\n' &&  c != '#' && c != EOF){
			switch (st)
			{
				case s1:
					if (c == ';' || c == '(' || c == ')' || c == '<')
					{
						str[0] = c;
						str[1] = '\0';
						lst = add_elem(lst, str);
						str = new_word();
						length = N;
						continue;
					}
					else 
						switch (c)
						{
							case '|': st = s3; break;
							case '&': st = s4; break;
							case '>': st = s5; break;
							case '"': st = s6; continue; break;
							default: st = s2; break;
						}
					if (k == length)
					{
						length *= 2;
						str = (char*) realloc(str, length+2);
					}
					if (c == '\\')
					{
						if ((c = getchar()) == EOF || c == '\n' || c == '#')
						{
							fprintf(stderr, "\n***syntax error***: after '\\' a symbol was expected\n");
							err = 1;
							return lst;
						}
					}
					str[k++] = c;
					break;
				case s2:
					if (c == ';' || c == ')' || c == '(' || c == '<')
					{
						str[k] = '\0';
						lst = add_elem(lst, str);
						str = new_word();
						str[0] = c;
						str[1] = '\0';
						lst = add_elem(lst, str);
						str = new_word();
						length = N;
						k = 0;
						st = s1;
					}	
					else
					{	
						switch (c)	
                	               	        {
                	                        	case '|': st = s3; break;
                	                      		case '&': st = s4; break;
                	                       	      	case '>': st = s5; break;
							case '"': st = s6; continue; break;
              	 	                       	      	default: if (k == length)
								 {
								 	length *= 2;
								 	str = (char*) realloc(str, length+2);
								 }
								 if (c == '\\')
								 {
									 if ((c = getchar()) == EOF || c == '\n' || c == '#')
									 {
										 fprintf(stderr,"\n***syntax error***: after '\\' a symbol was expected\n");
										 err = 1;
										 return lst;
									 }
								 }
								 str[k++] = c;
								 continue;
                              		        }
						if (st != s2)
						{
							str[k] = '\0';
							lst = add_elem(lst, str);
							str = new_word();
							length = N;
							k = 0;
							str[k++] = c;
						}
					}
					break;
				case s3:
					if (c == ';' || c == ')' || c == '(' || c == '<')
					{
						str[k] = '\0';
						lst = add_elem(lst, str);
						str = new_word();
						str[0] = c;
						str[1] = '\0';
						lst = add_elem(lst, str);
						str = new_word();
						length = N;
						k = 0;
						st = s1;
					}
					else if (c == '|')
						{
							str[k++] = c;
							str[k] = '\0';
							lst = add_elem(lst, str);
							str = new_word();
							length = N;
							k = 0;
							st = s1;
						}
						else
						{
							switch (c)
							{	
								case '&': st = s4; break;
								case '>': st = s5; break;
								case '"': st = s6; break;
								default: st = s2; break;
							}
							str[k] = '\0';
							lst = add_elem(lst, str);
							str = new_word();
							length = N;
							k = 0;
							if (st == s6) continue;
							if (c == '\\')
							{
								if ((c = getchar()) == EOF || c == '\n' || c == '#')
								{
									fprintf(stderr,"\n***syntax error***: after '\\' a symbol was expected\n");
									err = 1;
									return lst;
								}
							}
							str[k++] = c;
						}				
					break;
				case s4:
					if (c == ';' || c == ')' || c == '(' || c == '<')
					{
						str[k] = '\0';
						lst = add_elem(lst, str);
						str = new_word();
						str[0] = c;
						str[1] = '\0';
						lst = add_elem(lst, str);
						str = new_word();
						length = N;
						k = 0;
						st = s1;
					}
					else if (c == '&')
						{
							str[k++] = c;
							str[k] = '\0';
							lst = add_elem(lst, str);
							str = new_word();
							length = N;
							k = 0;
							st = s1;
						}
						else
						{
							switch (c)
							{	
								case '|': st = s3; break;
								case '>': st = s5; break;
								case '"': st = s6; break;
								default: st = s2; break;
							}
							str[k] = '\0';
							lst = add_elem(lst, str);
							str = new_word();
							length = N;
							k = 0;
							if (st == s6) continue;
							if (c == '\\')
							{
								if ((c = getchar()) == EOF || c == '\n' || c == '#')
								{
									fprintf(stderr,"\n***syntax error***: after '\\' a symbol was expected\n");
									err = 1;
									return lst;
								}
							}
							str[k++] = c;
						}				
					break;
				case s5:
					if (c == ';' || c == ')' || c == '(' || c == '<')
					{
						str[k] = '\0';
						lst = add_elem(lst, str);
						str = new_word();
						str[0] = c;
						str[1] = '\0';
						lst = add_elem(lst, str);
						str = new_word();
						length = N;
						k = 0;
						st = s1;
					}
					else if (c == '>')
						{
							str[k++] = c;
							str[k] = '\0';
							lst = add_elem(lst, str);
							str = new_word();
							length = N;
							k = 0;
							st = s1;
						}
						else
						{
							switch (c)
							{	
								case '&': st = s4; break;
								case '|': st = s3; break;
								case '"': st = s6; break;
								default: st = s2; break;
							}
							str[k] = '\0';
							lst = add_elem(lst, str);
							str = new_word();
							length = N;
							k = 0;
							if (st == s6) continue;
							if (c == '\\')
							{
								if ((c = getchar()) == EOF || c == '\n' || c == '#')
								{
									fprintf(stderr,"\n***syntax error***: after '\\' a symbol was expected\n");
									err = 1;
									return lst;
								}
							}
							str[k++] = c;
						}						
					break;
				case s6:
					if (c == '"')
					{
						st = s2;
						break;
					}
					str[k++] = c;
					while ((c = getchar()) != '"' && c != EOF && c != '\n')
					{
						if (k == length)
						{
							length *= 2;
						 	str = (char*) realloc(str, length+2);
						}
						str[k++] = c;
					}
					if (c == EOF || c == '\n')
					{
						fprintf(stderr,"\n***syntax error***: '\"' was expected\n");
						err = 1;
						return lst;
					}
					st = s2;
					break;
			}
		}
		else
		{
			switch (st)
			{
				case s2:
				case s3:
				case s4:
				case s5:
					st = s1;
					str[k] = '\0';
					lst = add_elem(lst, str);
					str = new_word();
					length = N;
					k = 0;
					break;
			}
		}
		if (c == '#')
		{
			while (c != EOF && c != '\n') c = getchar();
			return lst;
		}
		if (c == EOF || c == '\n') return lst;
	} while(1);
}

void shign(int s)
{
	printf("\nmyshell :) $ ");
	fflush(stdout);
	return;
}

void ignore(int s)
{
	putchar('\n');
	return;
}

int main(void)
{
	list* l = NULL;
	tree* t = NULL;
	signal(SIGINT, shign);
	printf("\nMYSHELL is started\n\n");
	do
	{
		t = clear_tree(t);
		l = clear_list(l);
		err = 0;
		l = build_list();
//		print_list(l);
		t = build_tree(l);
		if (!err)
		{
			print_tree(t);
			signal(SIGINT, ignore);
			execute(t);
			signal(SIGINT, shign);
		}
	} while (c != EOF);
	t = clear_tree(t);
	l = clear_list(l);
	printf("\n\nMYSHELL is ended\n\n");
	return 0;
}
