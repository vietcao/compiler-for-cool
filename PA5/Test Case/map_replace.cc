#include <stdio.h>
#include <map>

int main(){
  std::map<int, char*> mymap;
  mymap[1] = "123";
    mymap[2] = "345";
    mymap[1] = "666";
    printf("%s", mymap[1]);
    return 1;
}
