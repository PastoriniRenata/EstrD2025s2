
#include <iostream>
using namespace std;
#include "fracciones.h"


string fraccionToString (Fraccion f){
    return to_string(f.numerador) + "/" + to_string( f.denominador);
}


int main(){
    Fraccion f1 = consFraccion(2,10);
    Fraccion f2 = consFraccion(25,5);


    int numeradorF1 = numerador(f1);
    int denominadorF1= denominador(f1);
    float divisionF1 = division(f1);
    float divisionF2 = division(f2);
    Fraccion multF1yF2 = multF(f1, f2);
    Fraccion simplificadaF1 = simplificada(f1);
    Fraccion simplificadaF2 = simplificada(f2);
    Fraccion sumaF1yF2 = sumF(f1, f2);


    cout << "El numerador de la fraccion " << fraccionToString(f1) << " es (valor esperado 2): " << numeradorF1 << endl;

    cout<<"---------------------------------------------------------------------------"<<endl;

    cout << "El denominador de la fraccion " << fraccionToString(f1) << " es (valor esperado 10): " << denominadorF1 << endl;

    cout<<"---------------------------------------------------------------------------"<<endl;

    cout << "El resultado de la divicion de la fraccion " << fraccionToString(f1) << " es (valor esperado 0,2): " << divisionF1 << endl;

    cout<<"---------------------------------------------------------------------------"<<endl;

    cout << "El resultado de la divicion de la fraccion " << fraccionToString(f2) << " es (valor esperado 5): " << divisionF2 << endl;

    cout<<"---------------------------------------------------------------------------"<<endl;

    cout << "El resultado de la multiplicacion de las fracciones " << fraccionToString(f1) << " y " << fraccionToString(f2) << " es (valor esperado 50/50): " << fraccionToString(multF1yF2) << endl;

    cout<<"---------------------------------------------------------------------------"<<endl;

    cout << "El resultado de simplificar la fraccion "  << fraccionToString(f1) << " es (valor esperado 1/5): " << fraccionToString(simplificadaF1) << endl;

    cout<<"---------------------------------------------------------------------------"<<endl;

    cout << "El resultado de simplificar la fraccion "  << fraccionToString(f2) << " es (valor esperado 5/1): " << fraccionToString(simplificadaF2) << endl;

    cout<<"---------------------------------------------------------------------------"<<endl;

    cout << "El resultado de la suma de las fracciones " << fraccionToString(f1) << " y " << fraccionToString(f2) << " es (valor esperado 260/50 ): " << fraccionToString(sumaF1yF2) << endl;

    return 0;
}