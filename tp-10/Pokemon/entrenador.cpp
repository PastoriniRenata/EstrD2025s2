#include <iostream>
using namespace std;
#include "entrenador.h"



//Una vez hecho eso, implementar la siguiente interfaz de Entrenador:
//¿¿¿PUEDO USAR LA INTERFAZ DE POKEMON COMO USUARIO???
Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon){
//Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve
//un entrenador.
    Entrenador e = new EntrenadorSt;
    e->nombre = nombre;
    e->pokemon = pokemon;
    e->cantPokemon = cantidad;

    return e;
};


string nombreDeEntrenador(Entrenador e){
//Devuelve el nombre del entrenador.
    return e->nombre;
};


int cantidadDePokemon(Entrenador e){
//Devuelve la cantidad de pokémon que posee el entrenador.
    return e->cantPokemon;
};


int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
//Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
    int cantDe = 0;
    for(int i = 0; i< e->cantPokemon; i++){
        if(e->pokemon[i] -> tipo == tipo){
            cantDe++;
        }
    }
    return cantDe;
};


Pokemon pokemonNro(int i, Entrenador e){
//Devuelve el pokémon número i de los pokémon del entrenador.
//Precondición: existen al menos i - 1 pokémon.
    // if(i>= e->cantPokemon){
    //     return #error "Se rompe la precondicion";
    // }

    return e->pokemon[i];
};


bool algunoLeGanaA(Pokemon* pks, int cantPokes, Pokemon p){
    int i = 0;

    while(i<cantPokes and not  superaA(pks[i], p)){
        i++;
    }

    return i<cantPokes; // chequeo no haber llegado al final
}

bool leGanaATodos(Entrenador e1, Entrenador e2){
//Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero
//posee al menos un pokémon que le gane.
    bool rta = true;

    for(int i = 0; i<e2->cantPokemon and rta; i++){
        rta = rta and algunoLeGanaA(e1->pokemon, e1->cantPokemon , e2->pokemon[i]);
    }
    return rta;

};