#include <stdio.h>
#include <map>
#include <ostream>
#include "stringtab.h"

int main(){
  std::map<Symbol, Symbol> mymap;
  Symbol obj = idtable.add_string("Object");
  Symbol sym2 = idtable.add_string("sym2");
  Symbol sym3 = idtable.add_string("sym3");
  Symbol sym4 = idtable.add_string("sym4");
  Symbol sym5 = idtable.add_string("sym5");
  Symbol sym6 = idtable.add_string("sym6");
  Symbol sym7 = idtable.add_string("sym7");
  
  mymap[sym2] = obj;
  mymap[sym3] = sym2;
  mymap[sym4] = sym2;
  mymap[sym5] = sym4;
  mymap[sym6] = sym5;
  mymap[sym7] = sym6;
  int error = 0;
  Symbol test;
  for (std::map<Symbol,Symbol>::iterator it=mymap.begin(); it!=mymap.end(); ++it)
  {
    
    test = it->first;
    int i;
    Symbol parent;
    //parent = mymap[test];
    //tmp = parent;
      
    //cout << parent->get_string();
    
    for( i = 0; i < mymap.size()  ; i++){
      parent = mymap[test];
      
      
      if( parent->equal_string("Object", 6))
      {
	cout << "It's ok no cycle !";
	break;
      }else{
	test = parent;
      }
    }

    if( i == mymap.size())
    {
	error++;
    }
  }
  
  if( error != 0) cout << "EROR";
  
}
