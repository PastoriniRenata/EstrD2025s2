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
    int x = 4;
    int y = 8;
    cout << "La multiplicacion entre " << x << " y " << y << " es: " << x*y << " y tu codigo indica: " << mult(x, y) << endl;
    
    cout<<"---------------------------------------------------------------------------"<<endl;

    primerosN(3, "hola");

    cout<<"---------------------------------------------------------------------------"<<endl;
    char c = 'x';
    string s = "hola";
    cout << "El char " << c << ((pertenece(c,s))? " SI " : " NO ") << "pertenece al string " << s << endl;

    cout<<"---------------------------------------------------------------------------"<<endl;


    return 0;
}


