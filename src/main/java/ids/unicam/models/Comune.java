package ids.unicam.models;

import ids.unicam.controller.ContenutoController;
import ids.unicam.controller.ContestController;
import ids.unicam.controller.UtentiController;
import ids.unicam.models.attori.*;
import ids.unicam.utilites.Punto;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

public class Comune {

    private String nome;
    private Punto posizione;



    private final ArrayList<Curatore> curatori = new ArrayList<>();
    private GestorePiattaforma gestorePiattaforma ;
    private final ArrayList<Contributor> contributors = new ArrayList<>();
    private final Set<Animatore> animatori = new HashSet<>();
    private final ArrayList<TuristaLoggato> turisti = new ArrayList<>();
    private ContenutoController contenutoController;
    private ContestController contestController;
    private UtentiController utentiController;


    public ArrayList<TuristaLoggato> getTuristi() {
        return turisti;
    }
    public GestorePiattaforma getGestorePiattaforma() {
        return gestorePiattaforma;
    }

    public Set<Animatore> getAnimatori() {
        return animatori;
    }

    public Punto getPosizione() {
        return posizione;
    }

    public ArrayList<Contributor> getContributors() {
        return contributors;
    }

    public ArrayList<Curatore> getCuratori() {
        return curatori;
    }

    public String getNome() {
        return nome;
    }

    public Comune(String nome, Punto posizione, GestorePiattaforma gestorePiattaforma, ContenutoController contenutoController,ContestController contestController, UtentiController utentiController) {
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
        this.contenutoController=contenutoController;
        this.contestController=contestController;
        this.utentiController = utentiController;
    }
    public Comune(String nome, Punto posizione, GestorePiattaforma gestorePiattaforma){
        new Comune(nome,posizione,gestorePiattaforma,new ContenutoController(), new ContestController(),new UtentiController());
    }

    public ContenutoController getContenutoController() {
        return contenutoController;
    }

    public ContestController getContestController() {
        return contestController;
    }
    public UtentiController getUtentiController(){return utentiController;}

}
