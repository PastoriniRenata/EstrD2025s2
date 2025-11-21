#include <iostream>
using namespace std;
#include "arrayList.h"


struct ArrayListSt {
    int cantidad; // cantidad de elementos
    int* elementos; // array de elementos
    int capacidad; // tamaño del array
};


ArrayList newArrayList(){
//Crea una lista con 0 elementos.
//Nota: empezar el array list con capacidad 16.
    ArrayList aL = new ArrayListSt;
    aL->cantidad = 0 ;
    aL->capacidad =  16;    
    aL->elementos = new int[aL->capacidad] ;

    return aL;
};

ArrayList newArrayListWith(int capacidad){
//Crea una lista con 0 elementos y una capacidad dada por parámetro.
    ArrayList arrayL = new ArrayListSt;

    arrayL -> cantidad = 0;
    arrayL -> elementos = new int[capacidad];
    arrayL -> capacidad = capacidad;

    return arrayL;
};


int lengthAL(ArrayList xs){
//Devuelve la cantidad de elementos existentes.
    return xs->cantidad;
};


int get(int i, ArrayList xs){
//Devuelve el iésimo elemento de la lista.
    //PRECONDICION?? ERROR??
    
    return xs->elementos[i];
};


void set(int i, int x, ArrayList xs){
//Reemplaza el iésimo elemento por otro dado.
    xs->elementos[i] = x;

};


void resize(int capacidad, ArrayList xs){
//Decrementa o aumenta la capacidad del array.
//Nota: en caso de decrementarla, se pierden los elementos del final de la lista.
    if (xs->cantidad > capacidad){
        xs->cantidad = capacidad;
    }
    xs->capacidad=capacidad;
};


void add(int x, ArrayList xs){
//Agrega un elemento al final de la lista.
    xs->elementos[xs->cantidad] = x;
    xs->cantidad ++;

};


void remove(ArrayList xs){
//Borra el último elemento de la lista.
    xs->cantidad --; //cuando quiera agregar en esa posicion q estoy descartando, va a pisar el valor, xq esa posicion ahora la considero "basura"
};