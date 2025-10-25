#include <iostream>
using namespace std;
#include "tp-9.h"



/*
Ejercicio 4
Dar dos implementaciones para las siguientes funciones, una iterativa y otra recursiva, y utilizando
la menor cantidad posible de variables. Recordar defnir subtareas en caso de que sea estrictamente
necesario.
*/

//iterativa
void printN(int n, string s){
//Propósito: imprime n veces un string s.
    while (n > 0){
        cout << s << endl;
        n --;
    };
};




void cuentaRegresiva(int n){
//Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
    while(n>=0){
        cout << n << endl;
        n--;
    }
};

void desdeCeroHastaN(int n){
//Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
    int x = 0;
    while(x <= n){
        cout<<x<<endl;
        x++;
    }

};



//----------------------------------------------------------------------------------------------------

int multPositivos(int n, int m){
    // precondicion: m>0 y n>0
    int res = 0;

    while (m>0){
        res += n;
        m--;
    }
    
    return res;
};

int mult(int n, int m){
//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).

    int res = 0;

    return  (n==0 or m==0) ? 0 : 
                        (n>0 and m>0) ? multPositivos(n,m):
                                (n<0 and m<0)?multPositivos(-n,-m):
                                    (n<0)? (- multPositivos(-n, m)): (- multPositivos(n, -m)) ;
};



void primerosN(int n, string s){
//Propósito: imprime los primeros n char del string s, separados por un salto de línea.
    for(int i =0; i<n; i++){
        cout<< s[i] << endl;
    }
};


//Precondición: el string tiene al menos n char.

bool pertenece(char c, string s){
//Propósito: indica si un char c aparece en el string s.
    int i = 0;
    while(s[i] != '\0' and s[i] != c){
        i++;
    }

    return s[i] != '\0';
};



int apariciones(char c, string s){
//Propósito: devuelve la cantidad de apariciones de un char c en el string s.
    
    int apariciones = 0;

    for(int i =0; s[i]!= '\0'; i++){
        if(s[i] ==c){
            apariciones += 1;
        }
    }
    
    return apariciones;
};
