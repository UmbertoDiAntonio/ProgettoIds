package ids.unicam.models.contenuti;

import ids.unicam.controller.ContestController;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaLoggato;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;

@Entity
public class Contest extends Contenuto {
    private boolean open;
    @OneToOne
    private ContestController controller=null;
    private String obiettivo=null;
    private Animatore author=null;
    private final ArrayList<TuristaLoggato> partecipanti = new ArrayList<>();
    private final ArrayList<Invito> inviti = new ArrayList<>();
    @ManyToMany
    private final ArrayList<Materiale> materiali = new ArrayList<>();
    private String nome=null;

    public Contest() {

    }

    @OneToOne
    public Animatore getAuthor() {
        return author;
    }

    public String getNome() {
        return nome;
    }

    @OneToMany
    public ArrayList<Invito> getInviti() {
        return inviti;
    }
    public void setOpen(boolean open) {
        this.open = open;
    }

    public String getObiettivo() {
        return obiettivo;
    }


    public ArrayList<Materiale> getMaterialiContest() {
        return materiali;
    }

    public boolean isOpen() {
        return open;
    }

    @OneToMany
    public List<TuristaLoggato> getPartecipanti() {
        return partecipanti;
    }


    @OneToOne
    public ContestController getContestController() {
        return controller;
    }



    public Contest(String nome, boolean open, ContestController controller, String obiettivo, Animatore author) {
        super();
        setApproved(true);
        this.open = open;
        this.controller = controller;
        this.obiettivo = obiettivo;
        this.author = author;
        this.nome = nome;
    }



}
