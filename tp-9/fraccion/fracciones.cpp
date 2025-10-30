#include <iostream>
using namespace std;
#include "fracciones.h"



// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador){
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;

    return f;

}



// Propósito: devuelve el numerador
int numerador(Fraccion f){
    return f.numerador;
}

// Propósito: devuelve el denominador
int denominador(Fraccion f){
    return f.denominador;
}


// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f){
    return float(f.numerador) / f.denominador; // si no lo transformo en float no se hace bien la division
}


// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2){
    Fraccion f;
    f.numerador = f1.numerador * f2.numerador;
    f.denominador = f1.denominador * f2.denominador;

    return f;

}



// ------------------------------------------------------------------------------------------------------------------------

bool esDivisorDeAmbos (int n , int m, int divisor){
    int nI = n/ divisor;
    float nF = n/ divisor;

    int mI = m/divisor ;
    float mF = m/divisor ;

    return nI == nF and  mI == mF;
}

// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p){
    int menor = min(p.numerador, p.denominador);
    int mcd = menor;

    while (mcd>0 and not esDivisorDeAmbos(p.numerador, p.denominador, mcd) ){
        mcd -= 1;
    }

    Fraccion f;
    f.numerador = p.numerador / mcd;
    f.denominador = p.denominador / mcd;

    return f;


}



// Propósito: devuelve la fracción resultante de sumar las fracciones
Fraccion sumF(Fraccion f1, Fraccion f2){
    Fraccion res;
    res.numerador = f1.numerador * f2.denominador  + f2.numerador * f1.denominador;
    res.denominador = f1.denominador * f2.denominador;

    return res;
}


