package ids.unicam.models;

import ids.unicam.controller.ContenutoController;
import ids.unicam.controller.ContestController;
import ids.unicam.controller.UtentiController;
import ids.unicam.models.attori.*;
import ids.unicam.OSM.OSMRequester;
import ids.unicam.utilites.Punto;

import java.util.ArrayList;

public class Comune {

    private String nome;
    private Punto posizione;


    private final ArrayList<Curatore> curatori = new ArrayList<>();
    private GestorePiattaforma gestorePiattaforma;
    private final ArrayList<Contributor> contributors = new ArrayList<>();
    private final ArrayList<ContributorTrusted> contributorTrusteds = new ArrayList<>();
    private final ArrayList<Animatore> animatori = new ArrayList<>();
    private ContenutoController contenutoController;
    private ContestController contestController;


    public GestorePiattaforma getGestorePiattaforma() {
        return gestorePiattaforma;
    }

    public ArrayList<Animatore> getAnimatori() {
        return animatori;
    }

    public Punto getPosizione() {
        return posizione;
    }

    public ArrayList<Contributor> getContributors() {
        return contributors;
    }

    public ArrayList<ContributorTrusted> getContributorTrusteds() {
        return contributorTrusteds;
    }

    public ArrayList<Curatore> getCuratori() {
        return curatori;
    }

    public String getNome() {
        return nome;
    }

    public Comune(String nome, GestorePiattaforma gestorePiattaforma, ContenutoController contenutoController, ContestController contestController, UtentiController utentiController) {
        this.gestorePiattaforma = gestorePiattaforma;
        this.posizione = OSMRequester.getCentroComune(nome);
        this.nome = nome;
        if(!OSMRequester.getComuneAt(posizione).equalsIgnoreCase(nome))
            throw new IllegalArgumentException("Il nome del comune ricercato non corrisponde con nessun comune reale");
        this.contenutoController = contenutoController;
        this.contestController = contestController;
    }

    public Comune(String nome, GestorePiattaforma gestorePiattaforma) {
        new Comune(nome, gestorePiattaforma, new ContenutoController(), new ContestController(), new UtentiController());
    }

    public ContenutoController getContenutoController() {
        return contenutoController;
    }

    public ContestController getContestController() {
        return contestController;
    }

}
