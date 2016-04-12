#include <stdio.h>

class P {
public:
  P(){
  }
};

class C1 : public P {
public:
  C1(){
    P();
  }
};
class C2 : public P {
public:
  C2(){
  }
};

class A {
 private :
  P x;
 public :
  A() {
    x = new C1();
  }
};

int main(){
  return 1;
}
