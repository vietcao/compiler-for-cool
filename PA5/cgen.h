#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
// MY OWN CODE - V M Cao : inlcude map libary
#include <map>
enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   // MY OWN CODE - V M Cao : revert list is list in inherit form of nds
   List<CgenNode> *list_revert;

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
   
   // MY OWN CODE - V M Cao : define addition method 
   void create_class_name_table(); 
   void create_class_object_table();
   void create_dispatch_tables();
   void create_dispatch_table(List<CgenNode>* tmp_list, Symbol node_to_dispatch);
   void create_protypes();
   void create_protype(List<Entry>* tmp_type_decl, Symbol class_name, int class_tag);
   void create_init_methods();
   void create_class_methods();
   // build addtition data structure
   void build_list_revert();
   void build_tag_class_map();
   void build_class_attrs_map();
   void build_class_method_map();
   // End of code
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   Symbol get_name()   { return name; }		       
   Symbol get_parent() { return parent; }     	       
   Symbol get_filename() { return filename; } 
   Features get_features() {return features; }	
   //void dump_with_types(ostream&,int);                    
   // MY OWN CODE - V M Cao : declacate new methods
   List<CgenNode>* build_inherit_list();
   std::map< Symbol, List<Entry>* > build_method_fomals_map();
   List<attr_class>* build_list_attr();
   void build_attr_evi();
   void build_method_evi(Symbol method_name);
   void create_init_method(ostream &s);
   void create_methods(ostream &s, CgenClassTableP cgenTableP);
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

