#include <iostream>
using namespace std;
#include "tp-9.h"



/*
Ejercicio 4
Dar dos implementaciones para las siguientes funciones, una iterativa y otra recursiva, y utilizando
la menor cantidad posible de variables. Recordar denir subtareas en caso de que sea estrictamente
necesario.
*/


//---------------------------------------------------------------------------
void printN(int n, string s){
//Propósito: imprime n veces un string s.
    if (n > 0){
        cout << s << endl;
        printN(n-1, s);
    }
};

//---------------------------------------------------------------------------

void cuentaRegresiva(int n){
//Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
    if (n>=0){
        cout << n << endl;
        cuentaRegresiva(n-1);
    }
};
//---------------------------------------------------------------------------
void deXaN(int x, int n){
//funcion auxiliar de desdeCeroHastaN
    if(x <= n){
        cout << x << endl;
        deXaN(x+1, n);
    }
}
void desdeCeroHastaN(int n){
//Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
    deXaN(0, n);
};

//---------------------------------------------------------------------------

int multUnValorNegativo(int n, int m){
    
}

int losDosPositivos(int n, int m, int res){
    // precondicion: m>0 y n>0
    if(m>1){
        losDosPositivos(n, m-1, res+n);
    }

    return res;
}

int mult(int n, int m){
//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
//el q voy a recorrer es el m
    
    if(n==0 or m==0){
        return 0;
    }elif(n>0 and m>0){
        return losDosPositivos(n,m);
    }elif(n>0 and m<0){
        return 0
    }elif(n<0 and m<0){
        return - losDosPositivos(-n,-m);

    }
    
    
};
/*
//---------------------------------------------------------------------------

void primerosN(int n, string s){
//Propósito: imprime los primeros n char del string s, separados por un salto de línea.

};
//---------------------------------------------------------------------------

//Precondición: el string tiene al menos n char.

bool pertenece(char c, string s){
//Propósito: indica si un char c aparece en el string s.

};
//---------------------------------------------------------------------------

int apariciones(char c, string s){
//Propósito: devuelve la cantidad de apariciones de un char c en el string s.

};
*/