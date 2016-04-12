
#include "tree.h"
#include "cool-tree.h"
#include "stringtab.h"

int main(){
 Symbol Object = idtable.add_string("Object");
 Symbol Int = idtable.add_string("Int");
 Symbol Bool = idtable.add_string("Bool");
 Symbol filename = idtable.add_string("filename");


 Class_ Bool_class =
   class_(Bool, Object, nil_Features(),filename);
 Class_ Int_class =
	class_(Int, 
	       Object,
	       nil_Features(),
	       filename);

 
 Classes myclasses = append_Classes(append_Classes(single_Classes(Bool_class),single_Classes(Int_class)), single_Classes(Bool_class));
  printf("%s", "run over declecation\n");
  printf("%d, %s", myclasses->len(),"\n");
  int i;
  int length = 0 ;
  for ( i = 0; i < myclasses->len(); i++){
    Class_ myclass;
    
    myclass = myclasses->nth_length(i, length);
    
    Symbol parent = myclass->getName();
    printf("%s", parent->get_string());
  }
  return 1;
}





Classes nil_Classes()
{
   return new nil_node<Class_>();
}

Classes single_Classes(Class_ e)
{
   return new single_list_node<Class_>(e);
}

Classes append_Classes(Classes p1, Classes p2)
{
   return new append_node<Class_>(p1, p2);
}


Features nil_Features()
{
   return new nil_node<Feature>();
}

Features single_Features(Feature e)
{
   return new single_list_node<Feature>(e);
}



Class_ class_(Symbol name, Symbol parent, Features features, Symbol filename)
{
  return new class__class(name, parent, features, filename);
}

Class_ class__class::copy_Class_()
{
   return new class__class(copy_Symbol(name), copy_Symbol(parent), features->copy_list(), copy_Symbol(filename));
}

void class__class::dump(ostream& stream, int n)
{
  // stream << pad(n) << "class_\n";
  // dump_Symbol(stream, n+2, name);
  // dump_Symbol(stream, n+2, parent);
  // features->dump(stream, n+2);
  //dump_Symbol(stream, n+2, filename);
}
void class__class::dump_with_types(ostream& stream, int n){
}
/*
Symbol getParent(){
  return class__class this->getParent();
  }

class__class Class__class::cast(){
  
}*/
