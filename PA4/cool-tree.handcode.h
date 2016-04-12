//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H
 
#include <iostream>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

// MY OWN CODE - V M Cao : define TreeVistor and it's child

class program_class;
class class__class;
class method_class;
class attr_class;
class formal_class;
class branch_class;
class assign_class;
class static_dispatch_class;
class dispatch_class;
class cond_class;
class loop_class;
class typcase_class;
class block_class;
class let_class;
class plus_class;
class sub_class;
class mul_class;
class divide_class;
class neg_class;
class lt_class;
class eq_class;
class leq_class;
class comp_class;
class int_const_class;
class bool_const_class;
class string_const_class;
class new__class;
class isvoid_class;
class no_expr_class;
class object_class;

class TreeVisitor
{
public:
     
   virtual void visit(program_class& program)		{}
   virtual void visit(class__class& class_)	            {}   
   virtual void visit(method_class& method)	                { }
   virtual void visit(attr_class& attr)	                        { }
   virtual void visit(formal_class& formal)	                { }
   virtual void visit(branch_class& branch)	                { }
   virtual void visit(assign_class& assign)	                { }
   virtual void visit(static_dispatch_class& dispatch)	        { }
   virtual void visit(dispatch_class& dispatch)	                { }
   virtual void visit(cond_class& cond)	                        { }
   virtual void visit(loop_class& loop)	                        { }
   virtual void visit(typcase_class& typecase)	                { }
   virtual void visit(block_class& block)	                { }
   virtual void visit(let_class& let)	                        { }
   virtual void visit(plus_class& plus)	                        { }
   virtual void visit(sub_class& sub)	                        { }
   virtual void visit(mul_class& mul)	                        { }
   virtual void visit(divide_class& divide)	                { }
   virtual void visit(neg_class& neg)	                        { }
   virtual void visit(lt_class& lt)	                        { }
   virtual void visit(eq_class& eq)	                        { }
   virtual void visit(leq_class& leq)	                        { }
   virtual void visit(comp_class& comp)	                        { }
   virtual void visit(int_const_class& val)	                { }
   virtual void visit(bool_const_class& val)	                { }
   virtual void visit(string_const_class& val)	                { }
   virtual void visit(new__class& new_)	                        { }
   virtual void visit(isvoid_class& val)	                { }
   virtual void visit(no_expr_class& expr)	                { }
   virtual void visit(object_class& object)	                { }
};

class TableVisitor : public TreeVisitor{
public:
   virtual void visit(program_class& );		
   virtual void visit(class__class& );                
   virtual void visit(method_class& );	                
   virtual void visit(attr_class& );  
   virtual void visit(formal_class&);
};

class IdCheckVisitor : public TreeVisitor{
public :
   virtual void visit(program_class& );		
   virtual void visit(class__class& );	                
   virtual void visit(method_class& );	                
   virtual void visit(attr_class& )	;                  
   virtual void visit(formal_class& );	                
   virtual void visit(branch_class& );	
   virtual void visit(typcase_class& ); 
   virtual void visit(let_class& );          
   virtual void visit(assign_class& ); 			  
   virtual void visit(static_dispatch_class&);	 
   virtual void visit(dispatch_class& );      
   virtual void visit(cond_class& );	        	           
   virtual void visit(loop_class& );                       	                
   virtual void visit(block_class& );	               	                     
   virtual void visit(plus_class& );                 
   virtual void visit(sub_class& );                       
   virtual void visit(mul_class& );                    
   virtual void visit(divide_class& );	               
   virtual void visit(neg_class& );	                     
   virtual void visit(lt_class& );                      
   virtual void visit(eq_class& );	                       
   virtual void visit(leq_class&);	                       
   virtual void visit(comp_class& );	                      
   virtual void visit(int_const_class& );	               
   virtual void visit(bool_const_class& );	                 
   virtual void visit(string_const_class& );	                
   virtual void visit(new__class& );	                         
   virtual void visit(isvoid_class& );	                
   virtual void visit(no_expr_class& );	             
   virtual void visit(object_class& );	                 
 
};

 // add all accept and friend class to all define marco
#define Program_EXTRAS                          \
virtual void semant() = 0;			\
virtual void dump_with_types(ostream&, int) = 0; \
friend class TableVisitor; \
virtual void accept(TreeVisitor&) = 0;


#define program_EXTRAS                          \
void semant();     				\
void dump_with_types(ostream&, int);   \
friend class TableVisitor;                           \
friend class IdCheckVisitor;\
void accept(TreeVisitor& visitor) { visitor.visit(*this); }         

#define Class__EXTRAS                   \
virtual Symbol get_filename() = 0;      \
friend class TableVisitor; \
friend class IdCheckVisitor;\
virtual void dump_with_types(ostream&,int) = 0; \
virtual void accept(TreeVisitor&) = 0; 


#define class__EXTRAS                                 \
Symbol get_filename() { return filename; }             \
void dump_with_types(ostream&,int);    \
friend class TableVisitor;                           \
friend class IdCheckVisitor;\
void accept(TreeVisitor& visitor) { visitor.visit(*this); }                


#define Feature_EXTRAS                                        \
virtual void dump_with_types(ostream&,int) = 0; \
friend class TableVisitor;\
friend class IdCheckVisitor;\
virtual void accept(TreeVisitor&) = 0;


#define Feature_SHARED_EXTRAS                                       \
void dump_with_types(ostream&,int);    \
friend class TableVisitor;                           \
friend class IdCheckVisitor;\
void accept(TreeVisitor& visitor) { visitor.visit(*this); }





#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0; \
friend class IdCheckVisitor;\
virtual void accept(TreeVisitor&) = 0;


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int); \
friend class TableVisitor;                           \
friend class IdCheckVisitor;\
void accept(TreeVisitor& visitor) { visitor.visit(*this); }


#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0; \
friend class IdCheckVisitor;\
virtual void accept(TreeVisitor&) = 0;


#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int); \
friend class TableVisitor;                           \
friend class IdCheckVisitor;\
void accept(TreeVisitor& visitor) { visitor.visit(*this); }


#define Expression_EXTRAS                    \
virtual Symbol get_type() = 0;         \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; } \
friend class IdCheckVisitor;\
virtual void accept(TreeVisitor&) = 0;

#define Expression_SHARED_EXTRAS           \
void dump_with_types(ostream&,int); \
friend class TableVisitor;                           \
void accept(TreeVisitor& visitor) { visitor.visit(*this); } \
friend class IdCheckVisitor;\
Symbol get_type() { return type; }
#endif
