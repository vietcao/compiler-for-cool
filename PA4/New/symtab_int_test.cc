#include <stdio.h>
#include "symtab.h"
int main(){
  SymbolTable<char*, int > table = new SymbolTable<char*, int>();
  table->enterscope();
  table->addid("viet", new  int(23));
  int x = *table.lookup("viet");
  printf("%d", x);
	 return 1;
}
