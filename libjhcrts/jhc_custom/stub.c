#include "jhc_rts_header.h"
FILE* stderr = 0;
FILE* stdout = 0;
void abort() {
  printk("abort\n");
  stack_walk();
  for (;;);
}

void exit(int status) {
  printk("exit\n");
  abort();
}

void *posix_memalign(size_t __alignment, size_t __size) {
  abort();
}

char *setlocale(int category, const char *locale) {
  return NULL;
}

int fputs(const char *s, FILE *stream) {
  printk("%s", s);
  return 0;
}

int fprintf(FILE *stream, const char *format, ...) {
  return 0;
}

int fflush(FILE* stream) {
  return 0;
}

void A_COLD jhc_print_profile(void){}

int putchar(int c){
  printk("%c", c);
  return 0;
}

void show_int(uint32_t n){
  printk("[%d]\n", n/1000);
}
