#include <iostream>
using namespace std;
#include "ll.h"


struct NodoL {
    int elem; // valor del nodo
    NodoL* siguiente; // puntero al siguiente nodo
};

struct LinkedListSt {
    // INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer
    // desde primero por siguiente hasta alcanzar a NULL
    int cantidad; // cantidad de elementos
    NodoL* primero; // puntero al primer nodo
};


struct IteratorSt {
    NodoL* current;
};




LinkedList nil(){
    //Crea una lista vacía.
    LinkedListSt* l = new LinkedListSt;
    l-> cantidad = 0;
    l-> primero = NULL;
    return l ;
};

bool isEmpty(LinkedList xs){
    //Indica si la lista está vacía.
    return xs-> cantidad == 0;

};

int head(LinkedList xs){
    //PRECONDICION: xs tiene al menos 1 elemento
    //Devuelve el primer elemento.

    return xs->primero->elem;
};

void Cons(int x, LinkedList xs){
    //Agrega un elemento al principio de la lista.
    NodoL* n = new NodoL;
    n->elem = x;
    n->siguiente = xs->primero;

    xs->primero = n;
    xs->cantidad += 1;

};

void Tail(LinkedList xs){
    //PRECONDICION: tiene al menos 1 elementos
    //Quita el primer elemento.

    NodoL* aBorrar = xs->primero;
    xs->primero = aBorrar->siguiente;
    xs->cantidad -= 1;

    delete aBorrar;
};



int length(LinkedList xs){
    //Devuelve la cantidad de elementos.
    return xs->cantidad;
};

void Snoc(int x, LinkedList xs){
    //Agrega un elemento al final de la lista.
    NodoL* n = new NodoL;
    n->elem = x;
    n->siguiente = NULL;

    if(xs->cantidad == 0){
        xs->primero = n;
    }else{
        NodoL* curr = xs->primero;
        while(curr->siguiente != NULL){
            curr = curr->siguiente;
        }

        curr->siguiente = n;

    }
    xs->cantidad +=1 ;

};


//ITERADOR

ListIterator getIterator(LinkedList xs){
    //Apunta el recorrido al primer elemento.

    return ;
};

int current(ListIterator ixs){
    //Devuelve el elemento actual en el recorrido.
    return ;
};

void SetCurrent(int x, ListIterator ixs){
    //Reemplaza el elemento actual por otro elemento.
};

void Next(ListIterator ixs){
    //Pasa al siguiente elemento.
};

bool atEnd(ListIterator ixs){
    //Indica si el recorrido ha terminado.
    return ;
};

void DisposeIterator(ListIterator ixs){
    //Libera la memoria ocupada por el iterador.
};

void DestroyL(LinkedList xs){
    //Libera la memoria ocupada por la lista.
};