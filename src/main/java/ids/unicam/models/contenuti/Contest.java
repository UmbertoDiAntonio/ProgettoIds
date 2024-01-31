package ids.unicam.models.contenuti;

import ids.unicam.controller.ContestController;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaLoggato;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

public class Contest extends Contenuto {
    private boolean open;
    private final ContestController controller;
    private final String obiettivo;
    private final Animatore author;
    private final ArrayList<TuristaLoggato> partecipanti = new ArrayList<>();
    private final ArrayList<Invito> inviti = new ArrayList<>();
    private final HashMap<TuristaLoggato, Set<Materiale>> materialiContest = new HashMap<>();
    private final String nome;

    public Animatore getAuthor() {
        return author;
    }

    public String getNome() {
        return nome;
    }

    public ArrayList<Invito> getInviti() {
        return inviti;
    }
    public void setOpen(boolean open) {
        this.open = open;
    }

    public String getObiettivo() {
        return obiettivo;
    }

    public HashMap<TuristaLoggato, Set<Materiale>> getMaterialiContest() {
        return materialiContest;
    }

    public boolean isOpen() {
        return open;
    }

    public List<TuristaLoggato> getPartecipanti() {
        return partecipanti;
    }


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
