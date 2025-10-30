#include <iostream>
using namespace std;
#include "persona.h"


void imprimirPersona(Persona p){
    cout << nombre(p) << " y tiene ";
    cout << edad(p) << " años ";
};

int main(){

    Persona p1 = consPersona("Pedro", 26);
    Persona p2 = consPersona("Marta", 32);

    cout << "La primera persona es (valor esperado Pedro - 26 años):  " ;
    imprimirPersona(p1);
    cout<< endl;
    
    cout << "La primera persona es (valor esperado Marta - 32 años):  ";
    imprimirPersona(p2) ;
    cout<< endl;

    cout<<"---------------------------------------------------------------------------"<<endl;

    crecer(p1);
    cout << "Pedro creció y ahora tiene (valor esperado 27 años): "; 
    imprimirPersona(p1);
    cout<<endl;

    crecer(p2);
    cout << "Marta creció y ahora tiene (valor esperado 33 años): ";
    imprimirPersona(p2) ;
    cout<<endl;



    cout<<"---------------------------------------------------------------------------"<<endl;
    cambioDeNombre("José", p1);
    cout << "Pedro cambió de nombre y ahora se llama (valor esperado José - 27 años): "; 
    imprimirPersona(p1);
    cout<<endl;

    cambioDeNombre("Roberta", p2);
    cout << "Marta cambió de nombre y ahora se llama (valor esperado Roberta - 33 años ): ";
    imprimirPersona(p2) ;
    cout<<endl;


    cout<<"---------------------------------------------------------------------------"<<endl;
    
    cout << nombre(p1);

    if (esMayorQueLaOtra(p1, p2)){
        cout<< " es MAYOR a " ;
    }else{
        cout<< " es MENOR a " ;
    }

    cout<< nombre(p2) << endl;

    if (esMayorQueLaOtra(p2, p1)){
        cout<< "es MAYOR a " ;
    }else{
        cout<< "es MENOR a " ;
    }
    imprimirPersona(p1);
    cout<< endl;

    cout<<"---------------------------------------------------------------------------"<<endl;
    cout<< "El mayor entre ";
    cout<< nombre(p1) << " y " << nombre(p2) << " es " << nombre(laQueEsMayor(p1,p2)) << endl;
    
    cout<< endl;
    cout<< endl;


    return 0;
}
