#include <stdio.h>

class A{

};

class B : public A {
public:
  int get(){
    return 3;
  }
};
class C : public A {

};
int main(){
  A *c = new B;
  printf("%d",c->get());
  return 1;
}
