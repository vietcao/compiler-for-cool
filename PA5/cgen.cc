
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
// MY OWN CODE - V M Cao : include standar map data structure
#include <map>
extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;
extern char *curr_filename;
//My OWN CODE - V M Cao : delclaration globle data
CgenClassTable* codegen_classtable;
int no_lable = 0;
int sp_fp_offset = 0; 		// store offset of sp to fp to index the offset to store new id variable   
SymbolTable<Symbol, int>* attr_table;
SymbolTable<Symbol, int>* method_table;
std::map< Symbol, List<attr_class>* > class_attrs_map;
std::map< int,    Symbol > tag_class_map;
std::map< Symbol, 	int  > class_tag_map;
std::map< Symbol, std::map<Symbol, List<Entry>* > > class_method_map;
std::map< Symbol, std::map<Symbol, int > > class_method_offset_map;
CgenNodeP curr_class;
//std::map< Symbol, List<Symbol>* > method_formals_map; 

// End of code

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  codegen_classtable = new CgenClassTable(classes,os);
  codegen_classtable->code();

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/

      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/

      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/

      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s), list_revert(NULL)
{
   // MY OWN CODE - V M Cao : set initial classtag 	
   stringclasstag = 4 /* Change to your String class tag here */; 
   intclasstag =    2 /* Change to your Int class tag here */;
   boolclasstag =   3 /* Change to your Bool class tag here */;
   // End of code

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();
   // MY OWN CODE get code() dispatch out of contrustor and coment out exitscope()
   
	 //exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
	// build global data to create functions use
	build_list_revert(); 
	build_class_attrs_map();
	build_class_method_map();
	build_tag_class_map();
	if (cgen_debug) cout << "coding constants" << endl;
	create_class_name_table();
	create_class_object_table();
	create_dispatch_tables();
	create_protypes();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...
	create_init_methods();
	create_class_methods();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s) {
	expr->code(s);
	int offset;
	if( method_table->lookup(name) != NULL){
		offset = *method_table->lookup(name);
		emit_store(ACC, offset, FP, s);
	}else{
		offset = *attr_table->lookup(name);
		emit_load(ACC, offset, SELF, s);
	}
}

void static_dispatch_class::code(ostream &s) {
	int count = 0;
	for( int index = actual->first(); actual->more(index); index = actual->next(index) ){
		actual->nth(index)->code(s);
		emit_push(ACC, s);
		count++;
	}
	expr->code(s);
	emit_bne(ACC, ZERO, no_lable, s);
	emit_load_string(ACC , stringtable.lookup_string(curr_filename), s);
	emit_load_imm( T1, 1 ,s );
	emit_jal("_dispatch_abort", s);
	emit_label_def(no_lable++, s);
	
	for( int i = 0; i < count; i++ ){
		emit_load(T1, i*4, SP, s);
		emit_push(T1, s);
	}
	
	emit_load(T1, DISPTABLE_OFFSET*WORD_SIZE, ACC, s);
	std::map<Symbol, int> method_offset = class_method_offset_map[ type_name ]; // find via type_name specific
	int offset = method_offset[name] ;
	emit_load(T1, offset, T1, s); 
	emit_jalr(T1, s);
	emit_addiu(SP , SP, 4, s);
}

void dispatch_class::code(ostream &s) {
	int count = 0;
	for( int index = actual->first(); actual->more(index); index = actual->next(index) ){
		actual->nth(index)->code(s);
		emit_push(ACC, s);
		count++;
	}
	expr->code(s);
	emit_bne(ACC, ZERO, no_lable, s);
	emit_load_string(ACC , stringtable.lookup_string(curr_filename), s);
	emit_load_imm( T1, 1 ,s );
	emit_jal("_dispatch_abort", s);
	emit_label_def(no_lable++, s);
	
	for( int i = 0; i < count; i++ ){
		emit_load(T1, i*4, SP, s);
		emit_push(T1, s);
	}
	emit_load(T1, DISPTABLE_OFFSET*WORD_SIZE, ACC, s);
	std::map<Symbol, int> method_offset = class_method_offset_map[expr->get_type() ]; // find via static type of expr
	int offset = method_offset[name] ;
	emit_load(T1, offset, T1, s); 
	emit_jalr(T1, s);
	emit_addiu(SP, SP ,4 ,s);
}

void cond_class::code(ostream &s) {
	pred->code(s);
	emit_load(T1, 12, ACC , s);
	emit_beqz(T1, no_lable, s);
	then_exp->code(s);
	emit_branch( no_lable +1, s);	// branch to label for rest of code follow if expression
	emit_label_def(no_lable, s );
	else_exp->code(s);
	emit_label_def( no_lable++, s);
}

void loop_class::code(ostream &s) {
	emit_label_def(no_lable++, s);
	pred->code(s);
	emit_load(T1, 12, ACC , s);
	emit_beqz(T1, no_lable, s);	// branch to the rest if condition false
	body->code(s);
	emit_branch( no_lable -1 , s);
	emit_label_def(no_lable++ , s);
	emit_move(ACC, ZERO, s);
}
// 
// name: unknown
// @param : code_gen_classtable - to build a list_ordered class in BFS maner
// @return : print each label for each branch in order of list_ordered class - the order class in deep level in class inherences tree will go first
// @operate : First build a ordered_list of class name in order occur in inherit tree for all type_decl of branchs
//			  Generate code for expr and catch error when case on void expression
//			  loop over the list_ordered and generate code for all expr in each branch crosspone 			  
void typcase_class::code(ostream &s) {
	List<Entry>* original_list = NULL;
	CgenNodeP queue[1000];
	int head = 0;
	int tail = 0;
	List<Entry>* ordered_list = NULL;
	
	for( int index = cases->first(); cases->more(index); cases->next(index) )
		original_list = new List<Entry>( cases->nth(index)->get_type_decl(), original_list);
	
	CgenNodeP curr_node = codegen_classtable->root();
	while( list_length(original_list) != list_length(ordered_list) ){
		List<CgenNode>* child_list = curr_node->get_children();
		List<CgenNode>* l;
		for( l = child_list; l != NULL; l= child_list->tl())
			queue[tail++] = l->hd();
			
		List<Entry>* l1;
		for( l1 = original_list; l1 != NULL; l1= original_list->tl() ){
			if( curr_node->get_name() == l1->hd() )
				ordered_list = new List<Entry>( l1->hd(), ordered_list);		
		}
		
		curr_node = queue[head++];
	}
	
	expr->code(s);
	emit_push(ACC, s);
	sp_fp_offset = sp_fp_offset - 4;
	int save_no_lable = no_lable++; // save next label for the rest after typecase expression
	emit_bne( ACC, ZERO, no_lable, s );
	emit_load_string(ACC , stringtable.lookup_string(curr_filename), s);
	emit_load_imm( T1, 1 ,s );
	emit_jal("_case_abort2", s);
	
	List<Entry>* l;
	for( l = ordered_list; l !=NULL; l = l->tl() ){
		for( int index = cases->first(); cases->more(index); cases->next(index) ){
			if( cases->nth(index)->get_type_decl() == l->hd() ){
				Symbol type_name = cases->nth(index)->get_type_decl();
				CgenNodeP exam_node = codegen_classtable->lookup(type_name);
				List<CgenNode>* child_list = exam_node->get_children();
				
				int min = class_tag_map[child_list->hd()->get_name()];
				int max = 0;		
				List<CgenNode>* l;
				for( l = child_list; l != NULL; l = l->tl() ){
					int tag = class_tag_map[l->hd()->get_name()];
					if( max < tag ) max = tag;
				}
				method_table->enterscope();
				method_table->addid(cases->nth(index)->get_identifier(), new int(sp_fp_offset) );
				emit_label_def(no_lable++, s);
				emit_load(T2, 0, ACC, s);	// get class tag
				emit_blti(T2, min, no_lable , s);
				emit_bgti(T2, max , no_lable , s);
				cases->nth(index)->get_expr()->code(s);
				method_table->exitscope();	// remove identifier off method_table
				emit_addiu(SP, SP, 4 ,s );	
				emit_branch(save_no_lable, s); // branch to the rest after simualate brach cases
				break;
			}	
		}
	}		
	emit_label_def(save_no_lable, s);
			
}
void block_class::code(ostream &s) {
	for( int index = body->first(); body->more(index); body->next(index) ){
		body->nth(index)->code(s);
	}
}

void let_class::code(ostream &s) {
	if ( init->get_type() == No_type){
		//Symbol type_decl = attr->get_attr_type();
		Symbol int_const_0 = inttable.add_string("0");
		Symbol string_const_empty = stringtable.add_string("");
		emit_load_imm(ACC, 0 ,s );
		if( type_decl == Int ) { s << LA << ACC <<" "<< int_const_0 << endl; }
		if( type_decl == Str)  { s << LA << ACC <<" "<< string_const_empty << endl;}
		if( type_decl == Bool) { s << LA << ACC <<" "; falsebool.code_ref(s); s << endl;}
	}else
		init->code(s);
	// push new identifer to method evi and evalate the body
	emit_push(ACC, s);										
	sp_fp_offset = sp_fp_offset - 4;					
	method_table->enterscope();							
	method_table->addid( identifier, new int(sp_fp_offset) );		
	
	body->code(s);
	method_table->exitscope();
}

void plus_class::code(ostream &s) {
	e1->code(s);
	emit_push(ACC, s);
	sp_fp_offset = sp_fp_offset - 4;
	e2->code(s);
	emit_load(T1, 4, SP, s);
	emit_load(T1, 12, T1, s);
	emit_load(T2, 12, ACC, s);
	s << LA << ACC << " ";		// load Int_proObj to $a0 to call Object.copy routine to create new Int object
	emit_protobj_ref(Int, s);
	s << endl;
	emit_jal("Object.copy", s);
	emit_add(T1, T1, T2, s);
	emit_store_int(T1, ACC, s );	
	emit_addiu(SP, SP, 4 ,s );
}

void sub_class::code(ostream &s) {
	e1->code(s);
	emit_push(ACC, s);
	sp_fp_offset = sp_fp_offset - 4;
	e2->code(s);
	emit_load(T1, 4, SP, s);
	emit_load(T1, 12, T1, s);
	emit_load(T2, 12, ACC, s);
	s << LA << ACC << " ";		// load Int_proObj to $a0 to call Object.copy routine to create new Int object
	emit_protobj_ref(Int, s);
	s << endl;
	emit_jal("Object.copy", s);
	emit_sub(T1, T1, T2, s);
	emit_store_int(T1, ACC, s );
	emit_addiu(SP, SP, 4 ,s );
}

void mul_class::code(ostream &s) {
	e1->code(s);
	emit_push(ACC, s);
	sp_fp_offset = sp_fp_offset - 4;
	e2->code(s);
	emit_load(T1, 4, SP, s);
	emit_load(T1, 12, T1, s);
	emit_load(T2, 12, ACC, s);
	s << LA << ACC << " ";		// load Int_proObj to $a0 to call Object.copy routine to create new Int object
	emit_protobj_ref(Int, s);
	s << endl;
	emit_jal("Object.copy", s);
	emit_mul(T1, T1, T2, s);
	emit_store_int(T1, ACC, s );
	emit_addiu(SP, SP, 4 ,s );
}

void divide_class::code(ostream &s) {
	e1->code(s);
	emit_push(ACC, s);
	sp_fp_offset = sp_fp_offset - 4;
	e2->code(s);
	emit_load(T1, 4, SP, s);
	emit_load(T1, 12, T1, s);
	emit_load(T2, 12, ACC, s);
	s << LA << ACC << " ";		// load Int_proObj to $a0 to call Object.copy routine to create new Int object
	emit_protobj_ref(Int, s);
	s << endl;
	emit_jal("Object.copy", s);
	emit_div(T1, T1, T2, s);
	emit_store_int(T1, ACC, s );
	emit_addiu(SP, SP, 4 ,s );
}

void neg_class::code(ostream &s) {
	e1->code(s);
	emit_load( T1, 12, ACC, s);
	emit_load_bool(ACC, truebool, s);
	emit_beqz( T1, no_lable, s);
	emit_load_bool(ACC, falsebool, s );
	emit_label_def(no_lable , s);
	no_lable++;
	emit_addiu(SP, SP, 4 ,s );
}

void lt_class::code(ostream &s) {
	e1->code(s);
	emit_push(ACC, s);
	sp_fp_offset = sp_fp_offset - 4;
	e2->code(s);
	emit_load(T1, 4, SP, s);
	emit_load(T1, 12, T1, s);
	emit_load(T2, 12, ACC, s);
	emit_load_bool(ACC, truebool, s);
	emit_blt(T1, T2, no_lable, s);
	emit_load_bool(ACC, falsebool, s);
	emit_label_def(no_lable, s);
	no_lable++;
	emit_addiu(SP, SP, 4 ,s );
}

void eq_class::code(ostream &s) {
	e1->code(s);
	emit_push(ACC, s);
	sp_fp_offset = sp_fp_offset - 4;
	e2->code(s);
	emit_load(T1, 4, SP ,s);
	emit_move(T2, ACC, s);
	emit_load_bool(ACC, truebool, s);
	emit_beq( T1, T2, no_lable, s);
	emit_load_bool(ACC, falsebool, s);
	emit_jal( "equality_test", s);
	emit_label_def(no_lable, s);
	no_lable++;
	emit_addiu(SP, SP, 4 ,s );	
}

void leq_class::code(ostream &s) {
	e1->code(s);
	emit_push(ACC, s);
	sp_fp_offset = sp_fp_offset - 4;
	e2->code(s);
	emit_load(T1, 4, SP, s);
	emit_load(T1, 12, T1, s);
	emit_load(T2, 12, ACC, s);
	emit_load_bool(ACC, truebool, s);
	emit_bleq(T1, T2, no_lable, s);
	emit_load_bool(ACC, falsebool, s);
	emit_label_def(no_lable, s);
	no_lable++;
	emit_addiu(SP, SP, 4 ,s );
}

void comp_class::code(ostream &s) {
	emit_jal("Object.copy", s);
	emit_load(T1, 12, ACC, s);
	emit_neg(T1, T1, s);
	emit_store(T1, 12, ACC ,s);
}

void int_const_class::code(ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
	if( type_name == SELF_TYPE ){
		emit_load_address( T1, CLASSOBJTAB, s);
		emit_load(T2, WORD_SIZE*TAG_OFFSET, SELF, s);
		emit_sll(T2, T2, 3, s); // shifl left 3 equal mutipul t2 * 2^3. it is offset from classObjTable to SELF_TYPE proObj
		emit_addu(T1, T1, T2, s);
		emit_move(T3, T1, s);	// save the addess of SELF_TYPE proObj cause it plus 4 will be Init function for that object 
		emit_load(ACC, 0 , T1, s);
		emit_jal("Object.copy", s);
		emit_load(T1, 4, T3, s);
		emit_jalr(T1, s );		
	}else{
		s << LA; emit_protobj_ref(type_name, s); s << endl;
		emit_jal("Object.copy", s);
		s << JAL; emit_init_ref( type_name, s); s << endl;
	} 
}

void isvoid_class::code(ostream &s) {
	e1->code(s);
	emit_move(T1, ACC, s);
	emit_load_bool(ACC, truebool, s);
	emit_beqz(T1, no_lable, s);
	emit_load_bool(ACC, falsebool, s);
	emit_label_def(no_lable, s);
	no_lable++;
}

// do nothing with no_expr not it will be hanle on attr and let node instead
void no_expr_class::code(ostream &s) {
}

// look up id in method_table first then if not there look for in attr_table
void object_class::code(ostream &s) {
	if( name ==  self ){
			emit_move( ACC, SELF, s);
	}else{
		int offset;
		if( method_table->lookup(name) != NULL){
			offset = *method_table->lookup(name);
			emit_load(ACC, offset, FP, s);
		}else{
			offset = *attr_table->lookup(name);
			emit_load(ACC, offset, SELF, s);
		}
	}
		
}


// 
// name:  V M Cao
// @param : nds
// @return : revert list directionn of nds the list_revet
// @change : revert list now service like a list of cgenNode according classtag in order 0...n;
void CgenClassTable::build_list_revert(){
	List<CgenNode>* list = NULL;
	CgenNodeP root = this->root();
	CgenNodeP queue[1000];
	int head = 0;
	int tail = 0;
	queue[head] = root;
	while( head <= tail){
		CgenNodeP curr_node = queue[head++];
		List<CgenNode>* child_list = curr_node->get_children();
		List<CgenNode>* l;
		for( l = child_list; l != NULL; l = l->tl() ){
			list = new List<CgenNode>(l->hd(), list);
			queue[++tail] = l->hd();
		}
	}
	
	// revert the list result so hd() will be Object CgenNode
	List<CgenNode>* l;
	for( l = list; l != NULL; l->tl() )
		list_revert = new List<CgenNode>(l->hd(), list_revert);	
}

void CgenClassTable::build_tag_class_map(){
	List<CgenNode>* l;
	int tag = 0;
	for( l = list_revert; l!= NULL; l= l->tl() ){
		tag_class_map[ tag ] = l->hd()->get_name();
		class_tag_map[ l->hd()->get_name() ] = tag;
		tag++;
	}	
}
void CgenClassTable::build_class_attrs_map(){
	List<CgenNode>* l;
	List<attr_class>* attr_list; 
	for( l = nds; l!= NULL; l = l->tl() ) {
		attr_list = l->hd()->build_list_attr();
		class_attrs_map[l->hd()->get_name()] =  attr_list;
	}
}
// when build method_map already the method list now in order it appear on dispatch table
//	then build method_offset by increace offset for methods along that order
void CgenClassTable::build_class_method_map(){
	List<CgenNode>* l;
	std::map< Symbol, List<Entry>* >  method_map;
	std::map<Symbol , int > method_offset_map;
	for( l = nds; l!= NULL; l = l->tl() ) {
		method_map = l->hd()->build_method_fomals_map();                
		class_method_map[l->hd()->get_name()] =  method_map;
		
		int offset = 0;
		method_offset_map = std::map<Symbol, int>();
		for( std::map< Symbol, List<Entry>* >::iterator it= method_map.begin(); it!= method_map.end(); ++it){
			method_offset_map[it->first] = offset;
			offset = offset + 4;
		}
		class_method_offset_map[l->hd()->get_name()] = method_offset_map;
	}
}


	
/* print out classNameTab to output code
* take List<CgenNode> as input and loop over all emlement and put all string const lable already have in order as classtag 
* of CgenNode - V M Cao
*/
void CgenClassTable::create_class_name_table(){
	str << CLASSNAMETAB << LABEL;	
	List<CgenNode>* l;		
	for( l = list_revert ; l != NULL; l= l->tl() ){		
			str << WORD;
			stringtable.lookup_string(l->hd()->get_name()->get_string())->code_ref(str);
			str << endl;
	}
}
// 
// name: V M Cao
// @param : list_revert
// @return : print out classObjTab to ouput code
void CgenClassTable::create_class_object_table(){
	str << CLASSOBJTAB << LABEL;
	List<CgenNode>* l;
	//CgenNodeP head;
	for( l = list_revert; l != NULL; l = l->tl()){
		str << WORD;
		emit_protobj_ref(l->hd()->get_name(), str);
		str << endl;
		str << WORD;
		emit_init_ref(l->hd()->get_name(), str);
		str << endl;
	}
}
// 
// name: V M Cao
// @param : list cgenNode nds
// @return : void - print out all each of dispatch table for  each class
// @operate : 	Move toward the inherits tree from each CgenNode until reach Ojbect
			//and create a tmp_list to hold list of CgenNode along the path inherits 
			
void CgenClassTable::create_dispatch_tables(){
	
	List<CgenNode>* l;
	List<CgenNode>* tmp_list;
	
	for( l= nds; l !=NULL; l = l->tl() ){
		Symbol node_to_dispatch = l->hd()->name;
		tmp_list = l->hd()->build_inherit_list();
		create_dispatch_table(tmp_list, node_to_dispatch);
	}	
}	

// 
// name: V M Cao
// @param :tmp_list -the list of Cgennode from the node to print dispatch table to Object node
		 //node_to_dispatch - the Symbol name of Cgennode to print with ( name of class )
// @return : void - print dispatch of one node 
// @operate :	Move along tmp_list to retrive all method from root Object if face one 
			//already there then replace to the old one and create a std:map<method_name, class_name>
			//std::map will automatic do replace by add new element have same key
void CgenClassTable::create_dispatch_table(List<CgenNode>* tmp_list, Symbol node_to_dispatch){
	std::map<Symbol, Symbol> result_map;
	List<CgenNode>* l;
	for( l= tmp_list; l !=NULL; l = l->tl() ){
		CgenNodeP curr_node = l->hd();
		for (int index = curr_node->get_features()->first(); curr_node->get_features()->more(index); index = curr_node->get_features()->next(index)){
			if( curr_node->get_features()->nth(index)->get_method_name() != NULL) result_map[curr_node->get_features()->nth(index)->get_method_name()] = curr_node->name;
		}	
	}
	emit_disptable_ref(node_to_dispatch, str);
	str << LABEL;
	for( std::map<Symbol,Symbol>::iterator it= result_map.begin(); it!= result_map.end(); ++it){
		str << WORD;
		emit_method_ref(it->second, it->first, str);
	}
}

// 
// name: V M Cao
// @param : list Cgenode nds
// @return : print all protype for all class
// @operate : 	Loop throut nds to create each nds protype 
//				retrive classname, classtag, and attr_list - the List of attr_class belog to current CgenNode
//				list_type_name - the list have all type declaration in order parent to chlid and oder top down decl but in revert direction
void CgenClassTable::create_protypes(){
	List<CgenNode>* l;
	List<Entry> * list_type_decl;
	int class_tag;
	for( l= nds; l !=NULL; l = l->tl() ){
		Symbol class_name = l->hd()->name;
		int class_tag = class_tag_map[class_name];
		
		List<attr_class>* attr_list = class_attrs_map[class_name];
		List<attr_class>* l2 ;
		for( l2= attr_list; l2 !=NULL; l2 = l2->tl())
				list_type_decl = new List<Entry>( l2->hd()->get_attr_name(), list_type_decl);

		create_protype(list_type_decl, class_name, class_tag);
	}	
}
// 
// name: V M Cao
// @param : type_decl_list - the list of all type declaration of attribute but in revert order
//			class_name - the Symbol repsent the name of class
//			class_tag - the offset of class in nds list from end of nds list (the Object node)
// @return : print out each Protype label include garage offset
// @operate : create a revert list of list_type_decl - just for print in the list attr in order parent to child
//			  if test for basic type of each attribute and print initialize value 
void CgenClassTable::create_protype(List<Entry>* list_type_decl, Symbol class_name, int class_tag){
	str << WORD << "-1" << endl;
	emit_protobj_ref(class_name, str);
	str << LABEL;
	// ad clas_tag search
	str << WORD << class_tag << endl;
	str << WORD << list_length(list_type_decl) + 3 << endl;
	str << WORD;
	emit_disptable_ref(class_name, str);
	str << endl;
	
	List<Entry>* l;
	List<Entry>* list_revert = NULL;
	for( l = list_type_decl ; l != NULL; l->tl() )
		list_revert = new List<Entry>(l->hd(), list_revert);
	
	Symbol int_const_0 = inttable.add_string("0");
	Symbol string_const_empty = stringtable.add_string("");
	for( l = list_revert; l != NULL; l->tl() ){
		Symbol type = l->hd();
		if( type == Int ) { str << WORD << int_const_0 << endl; continue; }
		if( type == Str)  { str << WORD << string_const_empty<< endl; continue; }
		if( type == Bool) { str << WORD; falsebool.code_ref(str); str << endl; continue; }
		str << WORD << 0 << endl;
	}
}
// 
// name: V M Cao
// @param : nds - list of CgenNode to loop along 
// @return : print all _init lable for all class
// @operate : 
void CgenClassTable::create_init_methods(){
	List<CgenNode>* l;
	for( l = nds ; l != NULL; l = l->tl()){
		CgenNodeP curr_node = l->hd();
		curr_node->build_attr_evi();
		method_table = new SymbolTable<Symbol, int>(); //just create empty method_table for init expr required
		method_table->enterscope();
		curr_node->create_init_method(str);
	}
}

// 
// name: V M Cao
// @param : nds
// @return : print out all method of non-basic class
// @operate :  Loop throught nds list and if CgenNode in this list not basic,
//			   then loop througt all features of this CgenNode and build pre-enviroments : 
//			   		+arrt_table	- the map table attribute name to offset from sefl object in $a0;
//					+method_table - the map table formal name and offset from $fb;
//			   as when gerate code will add id and offset of id form fb in method_table
//			   temporaries will be store in stack in know order and position when evalate one expression
void CgenClassTable::create_class_methods(){
	List<CgenNode>* l;
	for( l = nds ; l != NULL; l = l->tl()){
		CgenNodeP curr_node = l->hd();
		if( curr_node->basic() == 1 ) continue;
		
		curr_node->create_methods( str, this );
	}
}

// 
// name: V M Cao
// @param : the Cgennode itself
// @return : biuld atrr_table for that maping atrributes Symbol name with offset from self object of each class Cgennode
// @operate : loop over features list get out each atrribute name and increase offset along
void CgenNode::build_attr_evi(){
	attr_table = new SymbolTable<Symbol, int>();
	attr_table->enterscope();
	int offset = 12;
	for (int index = this->features->first(); this->features->more(index); index = this->features->next(index)){
		if( this->features->nth(index)->get_attr_name() != NULL) {
			attr_table->addid( this->features->nth(index)->get_attr_name(), new int(offset) );
			offset = offset +4 ;
		}	
	}	
}

// 
// name: V M Cao
// @param : class_name_method , methodname
// @return	: build a method_table contain all the argument and offset of them from $fb
void CgenNode::build_method_evi(Symbol method_name){
	std::map<Symbol, List<Entry>* > method_formals_map = class_method_map[this->name];
	List<Entry>* formals_list = method_formals_map[method_name];
	int offset = 12; // initial offset and also offset of 1_st argument
	
	List<Entry>* l;
	List<Entry>* formals_revet_list = NULL;
	for( l = formals_list; l !=NULL ; l= l->tl() ){
		formals_revet_list  = new List<Entry>(l->hd(), formals_revet_list);
	}
	
	method_table = new SymbolTable<Symbol, int>();
	method_table->enterscope();
	for( l = formals_revet_list; l != NULL; l = l->tl() ){
		method_table->addid(l->hd(), new int(offset) );
		offset = offset + 4;
	} 

}


// @return : the list of inherits CgeNode in order Object node go first
List<CgenNode>* CgenNode::build_inherit_list(){
	List<CgenNode>* tmp_list = NULL;
	CgenNodeP parent = this;
		do{
			tmp_list = new List<CgenNode>(parent , tmp_list);
			parent = parent->get_parentnd();
		}while( parent->get_name() != No_class);
	
	return tmp_list;	
 }
// 
// name: unknown
// @param
// @return : list of attr_class of CgenNode in order parent attr and top go first
List<attr_class>* CgenNode::build_list_attr(){
	List<attr_class>* result = NULL;
	List<CgenNode>* tmp_list = this->build_inherit_list();
	List<CgenNode>* l;
	for( l = tmp_list; l !=NULL; l = l->tl() ){
		CgenNodeP curr_node = l->hd();
		for (int index = curr_node->get_features()->first(); curr_node->get_features()->more(index); index = curr_node->get_features()->next(index)){
			if( curr_node->get_features()->nth(index)->get_attr() != NULL) 
				result = new List<attr_class>(curr_node->get_features()->nth(index)->get_attr(), result ) ;
		}	
		
	}
	// revert the order of attrbute class to order first attr and atrr belong to parent go first( head of the list )
	List<attr_class>* new_result = NULL;
	List<attr_class>* l1;
	for( l1 = result ; l1 != NULL; l1 = l1->tl() )
		new_result = new List<attr_class>(l1->hd(), new_result);	
	return new_result;
}
// 
// name: V M Cao
// @param : CgeNode itself
// @return : map of [methodname, List<fomalname>] of  a  CgenNode include from prant node
//			 if methodname duplicate it will be replcate automatic by std::map
// @change : // add a function to method to build method_offset 
std::map< Symbol, List<Entry>* >  CgenNode::build_method_fomals_map (){
	std::map< Symbol, List<Entry>* > method_formals_map = std::map< Symbol, List<Entry>* >();
	List<CgenNode>* tmp_list = this->build_inherit_list();
	List<CgenNode>* l;
	for( l = tmp_list ; l != NULL; l = l->tl() ){ 
		for (int index = l->hd()->get_features()->first(); l->hd()->get_features()->more(index); index = l->hd()->get_features()->next(index) ){
			if( l->hd()->get_features()->nth(index)->get_method_name() != NULL){
				Formals formals = l->hd()->get_features()->nth(index)->get_formals();
				List<Entry>* formals_name = NULL;
				for( int i = formals->first(); formals->more(i); i = formals->next(i) )
					formals_name = new List<Entry>(formals->nth(i)->get_formal_name(), formals_name );
					
				method_formals_map[this->get_features()->nth(index)->get_method_name()] = formals_name;
			}		
		}	
	}
	return method_formals_map;
}

// 
// name: V M Cao
// @param : attr_table - offset of each atrribute form self;
// @return : print out init label for CgenNode this
// @operate : print out frame code and invoke int_expr code
void CgenNode::create_init_method(ostream &s){
	int offset = 12;		// offset's first attribute from self object
	emit_init_ref(this->name, s );
	s << LABEL;
	emit_addiu(SP, SP, -12 ,s );
	emit_store(FP, 12, SP, s);
	emit_store(SELF, 8, SP, s);
	emit_store(RA, 4, SP , s);
	emit_move(SELF, ACC, s );
	
	if( this->parentnd->get_name() != No_class){
		s << JAL;
		emit_init_ref(this->parentnd->get_name(), s);
		s << endl;
	}
	

	List<attr_class>* attr_list = this->build_list_attr();
	List<attr_class>* l;
	for( l = attr_list; l != NULL; l = l->tl() ){
		attr_class* attr = l->hd();
		//attr.get_attr_init()->code(s);
		if ( attr->get_attr_init()->get_type() == No_type){
			Symbol type_decl = attr->get_attr_type();
			Symbol int_const_0 = inttable.add_string("0");
			Symbol string_const_empty = stringtable.add_string("");
			emit_load_imm(ACC, 0 ,s );
			if( type_decl == Int ) { s << LA << ACC <<" "<< int_const_0 << endl; }
			if( type_decl == Str)  { s << LA << ACC <<" "<< string_const_empty<< endl;}
			if( type_decl == Bool) { s << LA << ACC <<" "; falsebool.code_ref(s); s << endl;}
		}else{
			attr->get_attr_init()->code(s);
		}	
		emit_store(ACC, offset, SELF, s);
		offset = offset+4;
	}
	
	emit_move(ACC, SELF, s);
	emit_load(FP, 12, SP, s);
	emit_load(SELF, 8, SP ,s);
	emit_load(RA, 4 , SP ,s);
	emit_addiu(SP, SP, 12 ,s );
	s << RET << endl;
}
// 
// name: V M Cao
// @param : expr - the body of method to generate code
// @return : print out grenerated code for one object
void create_method(Expression expr, Symbol class_name, Symbol method_name, ostream &s){
	emit_method_ref(class_name, method_name, s);
	s << LABEL;
	
	emit_addiu(SP, SP, -12 ,s );
	emit_store(FP, 12, SP, s);
	emit_store(SELF, 8, SP, s);
	emit_store(RA, 4, SP , s);
	emit_move(SELF, ACC, s );
	
	expr->code(s);
	
	emit_load(FP, 12, SP, s);
	emit_load(SELF, 8, SP ,s);
	emit_load(RA, 4 , SP ,s);
	emit_addiu(SP, SP, 12 ,s );
	s << RET << endl;
}

// 
// name: V M Cao
// @param : CgeNode itself , attr_table, method_table
// @return : print all method of this CgenNode
// @operation : Loop over all feautures and get out method features
//				With each method featues build attr_table, method_table for it own
//				evalate method() with attr_table, method_table just build and expression body of each method
void CgenNode::create_methods(ostream &s, CgenClassTableP cgenTableP){
	
	this->build_attr_evi();
	
	
	for (int index = this->features->first(); this->features->more(index); index = this->features->next(index)){
				Symbol method_name = this->features->nth(index)->get_method_name();
				if( method_name != NULL ){	
					this->build_method_evi( method_name );
					curr_class = this;
					create_method( this->features->nth(index)->get_expr(), this->name, method_name, s);
				}				
	}

}
