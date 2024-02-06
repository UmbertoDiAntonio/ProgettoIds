package ids.unicam;

import ids.unicam.Exception.ConnessioneFallitaException;
import ids.unicam.OSM.RichiestaOSM;
import ids.unicam.controller.ComuneController;
import ids.unicam.controller.ContenutoController;
import ids.unicam.controller.ContestController;
import ids.unicam.models.Ruolo;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.HashMap;


@Entity
public class Comune {
    @Id
    private String nome = "";
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
    private GestorePiattaforma gestorePiattaforma = null;

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
     * @param nome               Nome del Comune
     * @param gestorePiattaforma il gestore della piattaforma
     * @throws IllegalArgumentException se il nome del comune non corrisponde a nessun comune
     * @throws RuntimeException         se non è possibile raggiungere il sistema OSM
     */
    public Comune(String nome, GestorePiattaforma gestorePiattaforma) {
        ComuneController.getInstance().listaComuni.add(this);
        this.gestorePiattaforma = gestorePiattaforma;
        try {
            this.posizione = RichiestaOSM.getCoordinateDaComune(nome);
            this.nome = RichiestaOSM.getComuneDaCoordinate(posizione);

            if (this.nome == null)
                throw new RuntimeException("Impossibile stabilire la connessione con il sistema OSM");

            if (!this.nome.equalsIgnoreCase(nome))
                throw new IllegalArgumentException("Il nome del comune ricercato non corrisponde con nessun comune reale");

        } catch (ConnessioneFallitaException e) {
            Main.logger.error("Connessione fallita durante il recupero delle coordinate o del nome del comune da OSM", e);
        }
    }


    public final boolean verificaCoordinateComune(PuntoInteresse puntoInteresse) {
        String nomeComune = RichiestaOSM.getComuneDaCoordinate(new Punto(puntoInteresse.getPt().getLatitudine(), puntoInteresse.getPt().getLongitudine()));

        if (nomeComune != null) {
            return nomeComune.equalsIgnoreCase(getNome());
        }
        return false;
    }
}
