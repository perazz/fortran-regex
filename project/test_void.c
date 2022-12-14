#include <stdio.h>

void make_void(char c) {
 (void) c;
 return
}


int main() {
 
 char c[2];
 c = "A";

 printf("c=%s \n",&c);
 make_void(c);
 printf("c=%s \n",&c);
 return 0;
};

