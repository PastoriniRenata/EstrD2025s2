#include "arrayList.h"

int sumatoria(ArrayList xs){
//Devuelve la suma de todos los elementos.
    int sumatoria = 0;
    for(int i = 0; i<lengthAL(xs); i++){
        sumatoria += get(i, xs);
    }

    return sumatoria;
};

void sucesores(ArrayList xs){
//Incrementa en uno todos los elementos.
    for(int i = 0; i<lengthAL(xs); i++){
        set(i, get(i,xs) + 1 , xs);
    }
};

bool pertenece(int x, ArrayList xs){
//Indica si el elemento pertenece a la lista.
    int i = 0;
    while( i<lengthAL(xs) and not (get(i,xs) == x)){
        i++;
    }
    return i<lengthAL(xs);
};

int apariciones(int x, ArrayList xs){
//Indica la cantidad de elementos iguales a x.
    int apariciones = 0;
    for(int i = 0; i<lengthAL(xs); i++){
        apariciones += (get(i, xs) == x)? 1 : 0; // si son iguales sumo 1, sino sumo 0
    }

    return apariciones;
};










ArrayList append(ArrayList xs, ArrayList ys){
//Crea una nueva lista a partir de la primera y la segunda (en ese orden).
    int cantElementos = lengthAL(xs) + lengthAL(ys);
    ArrayList zs = newArrayListWith(cantElementos);

    for(int i = 0; i<lengthAL(xs); i++){
        add(get(i,xs), zs);
    }

    for(int i = 0; i<lengthAL(ys); i++){
        add(get(i,ys), zs);
    }

    return zs;
};

int minimo(ArrayList xs){
//Devuelve el elemento más chico de la lista.

//PRECONDICION: hay al menos un elemento?????
    int min = get(0,xs);
    for(int i = 1; i<lengthAL(xs); i++){
        if(get(i,xs)<min){
            min = get(i,xs);
        }
    }

    return min;

};



//-------------------------------------------MAIN-----------------------------------------

// AUX. para mostrar la lista
void PrintList(ArrayList xs) {
    cout << "[" ;
    for (int i = 0; i < lengthAL(xs); i++) {
        cout << get(i, xs) << ", ";
    }
    cout << "]" << endl;
}



int main() {
    
    cout << "EJERCICIOS COMO USUARIO DEL TAD ARRAYLIST" << endl;
    cout << endl;

    ArrayList xs = newArrayList(); // capacidad 16
    add(1, xs);
    add(2, xs);
    add(3, xs);
    add(4, xs);
    add(5, xs);
    add(5, xs);

    int x = 6;


    ArrayList ys = newArrayList(); // capacidad 16
    add(10, ys);
    add(20, ys);
    add(30, ys);
    add(40, ys);
    add(50, ys);
    add(50, ys);


    cout << "La sumatoria de los elementos del ArrayList es (20) -->  " << sumatoria( xs) << endl;

    sucesores( xs); // --> xs = [2,3,4,5,6,6]
    cout << "El arrayList resultante de hacer sucesores al arrayList que se conforma por [1,2,3,4,5,5] es --> " ;
    
    PrintList (xs);
    
    if(pertenece(x,  xs)){
        cout << "El elemento "<< x<<" pertenece al array " << endl;
    }else{
        cout << "El elemento "<< x<<" NO pertenece al array " << endl;
    }    
    
    cout << "El elemento " << x << " aparece " << apariciones( x, xs) << " veces en el arrayList. (2)"   << endl;


    
    cout << "El resultado de hacerles apend a los arrayList: "; 
    PrintList(xs);
    cout << " y "; 
    PrintList(ys);
    cout<< " es ( [2,3,4,5,6,6,10,20,30,40,50] )   --> ";
    PrintList( append( xs,  ys)) ;
    cout << endl;



    cout << "El elemento más chico del arrayList es (2) :  " << minimo( xs) << endl;



    return 0;
}
