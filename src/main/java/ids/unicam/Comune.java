package ids.unicam;

import ids.unicam.Exception.ConnectionFailed;
import ids.unicam.OSM.OSMRequester;
import ids.unicam.controller.ComuneController;
import ids.unicam.controller.ContenutoController;
import ids.unicam.controller.ContestController;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import jakarta.persistence.*;

import java.util.ArrayList;


public class Comune {

    private String nome="";
    private Punto posizione;
    private GestorePiattaforma gestorePiattaforma=null;
    private final ArrayList<Curatore> curatori = new ArrayList<>();
    private final ArrayList<Contributor> contributors = new ArrayList<>();
    private final ArrayList<ContributorTrusted> contributorTrusteds = new ArrayList<>();
    private final ArrayList<Animatore> animatori = new ArrayList<>();
    private final ContenutoController contenutoController = new ContenutoController();
    private final ContestController contestController = new ContestController();
    private Long id;

    public Comune() {

    }



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
     * @param gestorePiattaforma  il gestore della piattaforma
     * @throws IllegalArgumentException se il nome del comune non corrisponde a nessun comune
     * @throws RuntimeException se non è possibile raggiungere il sistema OSM
     */
    public Comune(String nome, GestorePiattaforma gestorePiattaforma) {
        ComuneController.getInstance().listaComuni.add(this);
        this.gestorePiattaforma = gestorePiattaforma;
        try {
            this.posizione = OSMRequester.getCentroComune(nome);
        } catch (ConnectionFailed e) {
            e.printStackTrace();
        }
        this.nome = nome;
        String nomeComune = null;
        try {
            nomeComune = OSMRequester.getComuneAt(posizione);
        } catch (ConnectionFailed e) {
            e.printStackTrace();
        }
        if (nomeComune == null)
            throw new RuntimeException("Impossibile stabilire la connessione con il sistema OSM");
        if (!nomeComune.equalsIgnoreCase(nome))
            throw new IllegalArgumentException("Il nome del comune ricercato non corrisponde con nessun comune reale");
    }


    public final boolean checkCoordinateComune(PuntoInteresse puntoInteresse){
        String nomeComune = null;
        try {
            nomeComune = OSMRequester.getComuneAt(new Punto(puntoInteresse.getPt().getLatitudine(),puntoInteresse.getPt().getLongitudine()));
        } catch (ConnectionFailed e) {
            e.printStackTrace();
        }
        if(nomeComune != null) {
            return nomeComune.equalsIgnoreCase(getNome());
        }
        return false;
    }


    public void setId(Long id) {
        this.id = id;
    }

    
    @GeneratedValue
    public Long getId() {
        return id;
    }
}
