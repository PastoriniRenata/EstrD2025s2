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

int multPositivos(int n, int m){
    // precondicion: m>0 y n>0
    return (m>1) ? n + multPositivos(n, m-1) : n;

}

int mult(int n, int m){
//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
//el q voy a recorrer es el m
    return (n==0 or m==0) ? 0 : 
                    (n>0 and m>0) ? multPositivos(n,m):
                            (n<0 and m<0)? multPositivos(-n,-m):
                                (n<0)? (- multPositivos(-n, m)): (- multPositivos(n, -m)) ;
};

//---------------------------------------------------------------------------


void primerosNaux(int n,string s, int i){
    if (i < n and  s[i] != '\0'){
        cout << s[i] << endl ;
        primerosNaux(n, s, i+1); 
    };

};


void primerosN(int n, string s){
//Propósito: imprime los primeros n char del string s, separados por un salto de línea.
//Precondición: el string tiene al menos n char.

    primerosNaux(n,s,0);
};


//---------------------------------------------------------------------------


bool pertenece2 (char c, string s, int i){
    // bool esta = false;

    // if(s[i] != '\0'){
    //     esta = s[i] == c || pertenece2(c, s, i+1); 
    // };

    return s[i] != '\0' and (s[i] == c || pertenece2(c, s, i+1));
};


bool pertenece(char c, string s){
//Propósito: indica si un char c aparece en el string s.

    return pertenece2(c,s,0); 
  
};




/*

//---------------------------------------------------------------------------

int apariciones(char c, string s){
//Propósito: devuelve la cantidad de apariciones de un char c en el string s.

};
*/