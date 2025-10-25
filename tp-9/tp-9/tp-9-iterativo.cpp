#include <iostream>
using namespace std;
#include "tp-9.h"



/*
Ejercicio 4
Dar dos implementaciones para las siguientes funciones, una iterativa y otra recursiva, y utilizando
la menor cantidad posible de variables. Recordar denir subtareas en caso de que sea estrictamente
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


/*

int mult(int n, int m){
//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).

};

void primerosN(int n, string s){
//Propósito: imprime los primeros n char del string s, separados por un salto de línea.

};

//Precondición: el string tiene al menos n char.

bool pertenece(char c, string s){
//Propósito: indica si un char c aparece en el string s.

};

int apariciones(char c, string s){
//Propósito: devuelve la cantidad de apariciones de un char c en el string s.

};
*/