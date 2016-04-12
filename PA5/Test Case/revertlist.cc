#include <stdio.h>
#include "list.h"

int main(){
  List<int>* list = NULL;
  list = new List<int>(3, list);
  list = new List<int>(5, list);
  List<int>* l;
  List<int>* revert= NULL;
  for( l= list; l !=NULL; l = l->tl() ){
    revert = new List<int>(l->hd() ,revert);
  }
  return 1;
}
