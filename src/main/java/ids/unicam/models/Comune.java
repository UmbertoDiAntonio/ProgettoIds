package ids.unicam.models;

import ids.unicam.OSM.OSMRequester;
import ids.unicam.controller.ContenutoController;
import ids.unicam.controller.ContestController;
import ids.unicam.models.attori.*;
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

    public ContenutoController getContenutoController() {
        return contenutoController;
    }

    public ContestController getContestController() {
        return contestController;
    }

    public String getNome() {
        return nome;
    }

    /**
     * @param nome                Nome del Comune
     * @param gestorePiattaforma
     * @param contenutoController
     * @param contestController
     * @throws IllegalArgumentException se il nome del comune non corrisponde a nessun comune
     * @throws RuntimeException se non è possibile raggiungere il sistema OSM
     */
    public Comune(String nome, GestorePiattaforma gestorePiattaforma, ContenutoController contenutoController, ContestController contestController) {
        this.gestorePiattaforma = gestorePiattaforma;
        this.posizione = OSMRequester.getCentroComune(nome);
        this.nome = nome;
        String nomeComune = OSMRequester.getComuneAt(posizione);
        if (nomeComune == null)
            throw new RuntimeException("Impossibile stabilire la connessione con il sistema OSM");
        if (!nomeComune.equalsIgnoreCase(nome))
            throw new IllegalArgumentException("Il nome del comune ricercato non corrisponde con nessun comune reale");
        this.contenutoController = contenutoController;
        this.contestController = contestController;
    }

    /**
     *
     * @param nome nome del Comune
     * @param gestorePiattaforma
     *
     * @throws IllegalArgumentException se il nome del comune non corrisponde a nessun comune
     * @throws RuntimeException se non è possibile raggiungere il sistema OSM
     */
    public Comune(String nome, GestorePiattaforma gestorePiattaforma) {
        new Comune(nome, gestorePiattaforma, new ContenutoController(), new ContestController());
    }


}
