#include <iostream>
using namespace std;
#include "tp-9.h"


/*

ITERATIVO
g++ -o iterativo tp-9-main.cpp tp-9-iterativo.cpp
./iterativo           


RECURSIVO
g++ -o recursivo tp-9-main.cpp tp-9-recursivo.cpp
./recursivo      

*/
int main(){

    printN(3, "holaaa");
    cout<<"---------------------------------------------------------------------------"<<endl;

    cuentaRegresiva(4);
    cout<<"---------------------------------------------------------------------------"<<endl;

    desdeCeroHastaN(4);
    cout<<"---------------------------------------------------------------------------"<<endl;



    return 0;
}


