#include "stringtab.h"
#include "symtab.h"
#include <stdio.h>
#include <stdlib.h>

int main(){
  SymbolTable<Symbol,Symbol> *mytable = new SymbolTable<Symbol,Symbol>();
   Symbol Object = idtable.add_string("Object");
   Symbol Int = idtable.add_string("Int");
   Entry eInt = *Int;
   
   mytable->enterscope();
   mytable->addid(Object, &Int);
   cout << ((mytable->probe(Object) != NULL) ? "Yes\n" : "No\n");
   
   return 0;
}
