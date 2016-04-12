#include <stdio.h>
#include <map>
#include <ostream>
#include "stringtab.h"

 
int main(){
  std::map<char*, char*> mymap;
  //Symbol obj = idtable.add_string("Object");
  char* obj = "Object";
if ( mymap[obj] == NULL) printf("%s", "obj found");
return 1;
}
