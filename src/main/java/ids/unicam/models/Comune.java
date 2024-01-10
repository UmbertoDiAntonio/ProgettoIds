package ids.unicam.models;

import ids.unicam.controller.ContenutoController;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.attori.GestorePiattaforma;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.utilites.Punto;

import java.util.*;

public class Comune {
    private String nome;
    private Punto posizione;

//TODO mettodi per aggiungere e gestire contenuti

    private final Set<Curatore> curatori = new HashSet<>();
    private GestorePiattaforma gestorePiattaforma ;
    private final Set<Contributor> contributors = new HashSet<>();
    private final Set<Animatore> animatori = new HashSet<>();
    private ContenutoController contenutoController=ContenutoController.getInstance();

    public GestorePiattaforma getGestorePiattaforma() {
        return gestorePiattaforma;
    }

    public Set<Animatore> getAnimatori() {
        return animatori;
    }

    public Punto getPosizione() {
        return posizione;
    }

    public Set<Contributor> getContributors() {
        return contributors;
    }

    public Set<Curatore> getCuratori() {
        return curatori;
    }

    public String getNome() {
        return nome;
    }

    public Comune(String nome, Punto posizione, GestorePiattaforma gestorePiattaforma) {
        if(posizione==null){
            //TODO
            return;
        }
        if(!nome.matches("^[A-z0-9_]*$")){
            //TODO
            return;
        }
        this.gestorePiattaforma = gestorePiattaforma;
        this.posizione = posizione;
        this.nome=nome;
    }

    public ContenutoController getContenutoController() {
        return contenutoController;
    }
}
