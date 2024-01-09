package ids.unicam.models;

import ids.unicam.utilites.Punto;

import java.util.HashSet;
import java.util.Set;

public class Comune {
    private String nome;
    private Punto posizione;
    private Set<Contenuto> contenuti =new HashSet<>();

    public Set<Contenuto> getContenuti() {
        return contenuti;
    }
//TODO mettodi per aggiungere e gestire contenuti


    public Comune(String nome, Punto posizione) {
        if(posizione==null){
            //TODO
            return;
        }
        if(!nome.matches("^[A-z0-9_]*$")){
            //TODO
            return;
        }
        this.posizione = posizione;
        this.nome=nome;
    }
}
