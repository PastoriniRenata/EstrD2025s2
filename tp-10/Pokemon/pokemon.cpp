#include <iostream>
using namespace std;
#include "pokemon.h"



Pokemon consPokemon(TipoDePokemon tipo){
//Dado un tipo devuelve un pokémon con 100 % de energía.
    Pokemon p = new PokeSt;
    p -> tipo = tipo;
    p -> vida = 100;

    return p;
};


TipoDePokemon tipoDePokemon(Pokemon p){
//Devuelve el tipo de un pokémon.
    return p->tipo;
};


int energia(Pokemon p){
//Devuelve el porcentaje de energía.
    return p->vida;
};


void perderEnergia(int energia, Pokemon p){
//Le resta energía al pokémon.
    p->vida = p->vida - energia;
};


bool superaA(Pokemon p1, Pokemon p2){
//Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. 
//Agua supera a fuego, 
//fuego a planta y 
//planta a agua. 
//Y cualquier otro caso es falso.
    return (p1-> tipo == "Agua" and p2->tipo == "Fuego") or 
           (p1-> tipo == "Fuego" and p2->tipo == "Planta") or 
           (p1-> tipo ==  "Planta" and p2->tipo == "Agua");

};



