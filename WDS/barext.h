
int	BAR_vmain(char *arg, ...);
int	BAR_main(char *argv[]);
char	**BAR_make_argv(char *arg, ...);
void	BAR_free_argv(char **argv);
void	BAR_error(char *s);

/*  Supplied by application using BAR.LIB  */

void	BAR_message(char *s);
int	BAR_getch(void);
