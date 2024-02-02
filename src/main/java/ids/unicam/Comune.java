package ids.unicam;

import ids.unicam.Exception.ConnessioneFallitaException;
import ids.unicam.OSM.RichiestaOSM;
import ids.unicam.controller.ComuneController;
import ids.unicam.controller.ContenutoController;
import ids.unicam.controller.ContestController;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import jakarta.persistence.*;

import java.util.ArrayList;

@Entity
public class Comune {
    @Id
    private String nome="";
    @OneToOne
    private Punto posizione;
    @OneToMany
    private final ArrayList<Curatore> curatori = new ArrayList<>();
    @OneToMany
    private final ArrayList<Contributor> contributors = new ArrayList<>();
    @OneToMany
    private final ArrayList<ContributorAutorizzato> contributorAutorizzati = new ArrayList<>();
    @OneToMany
    private final ArrayList<Animatore> animatori = new ArrayList<>();
    @Transient
    private final ContenutoController contenutoController = new ContenutoController();
    @Transient
    private final ContestController contestController = new ContestController();
    @Transient
    private GestorePiattaforma gestorePiattaforma=null;

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


    public ArrayList<ContributorAutorizzato> getContributorAutorizzati() {
        return contributorAutorizzati;
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
            this.posizione = RichiestaOSM.getCoordinateDaComune(nome);
        } catch (ConnessioneFallitaException e) {
            e.printStackTrace();
        }
        this.nome = nome;
        String nomeComune = null;
        try {
            nomeComune = RichiestaOSM.getComuneDaCoordinate(posizione);
        } catch (ConnessioneFallitaException e) {
            e.printStackTrace();
        }
        if (nomeComune == null)
            throw new RuntimeException("Impossibile stabilire la connessione con il sistema OSM");
        if (!nomeComune.equalsIgnoreCase(nome))
            throw new IllegalArgumentException("Il nome del comune ricercato non corrisponde con nessun comune reale");
    }


    public final boolean verificaCoordinateComune(PuntoInteresse puntoInteresse){
        String nomeComune = null;
        try {
            nomeComune = RichiestaOSM.getComuneDaCoordinate(new Punto(puntoInteresse.getPt().getLatitudine(),puntoInteresse.getPt().getLongitudine()));
        } catch (ConnessioneFallitaException e) {
            e.printStackTrace();
        }
        if(nomeComune != null) {
            return nomeComune.equalsIgnoreCase(getNome());
        }
        return false;
    }
}
