package ids.unicam;

import ids.unicam.OSM.OSMRequester;
import ids.unicam.controller.ComuneController;
import ids.unicam.controller.ContenutoController;
import ids.unicam.controller.ContestController;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;

import java.util.ArrayList;

public class Comune {

    private String nome;
    private Punto posizione;
    private GestorePiattaforma gestorePiattaforma;
    private final ArrayList<Curatore> curatori = new ArrayList<>();
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
    //TODO passare come parametri i controller ha senso solo se sono interfacce
    public Comune(String nome, GestorePiattaforma gestorePiattaforma, ContenutoController contenutoController, ContestController contestController) {
        ComuneController.getInstance().listaComuni.add(this);
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


    public final boolean checkCoordinateComune(PuntoInteresse puntoInteresse){
        String nomeComune = OSMRequester.getComuneAt(new Punto(puntoInteresse.getPt().getLatitudine(),puntoInteresse.getPt().getLongitudine()));
        if(nomeComune != null) {
            return nomeComune.equalsIgnoreCase(getNome());
        }
        return false;
    }



}
