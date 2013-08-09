#ifndef _STDIO_H_
#define _STDIO_H_
typedef void* FILE;
extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;
int getchar(void);
int getc(FILE *stream);
int putc(int c, FILE *stream);
int putchar(int c);
int fprintf(FILE *stream, const char *format, ...);
char *getenv(const char *name);
int fflush(FILE *stream);
int fputs(const char *s, FILE *stream);
#endif
