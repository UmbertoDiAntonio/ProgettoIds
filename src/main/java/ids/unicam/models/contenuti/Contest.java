package ids.unicam.models.contenuti;

import ids.unicam.controller.ContestController;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaLoggato;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

public class Contest {
    private boolean open;
    private final ContestController controller;
    private final String obiettivo;
    private final Animatore author;
    private final String id ;

    public Animatore getAuthor() {
        return author;
    }

    public Contest(boolean open, ContestController controller, String obiettivo, Animatore author) {
        this.id = ContestController.generateID();
        this.open = open;
        this.controller = controller;
        this.obiettivo = obiettivo;
        this.author = author;
    }

    public String getId() {
        return id;
    }

    public void setOpen(boolean open) {
        this.open = open;
    }


    private final List<TuristaLoggato> invitati=new ArrayList<>();

    public String getObiettivo() {
        return obiettivo;
    }

    //Multimap<String,String> map = ArrayListMultimap.create();
    private final HashMap<TuristaLoggato, Set<Materiale>> materialiContest = new HashMap<>();

    public HashMap<TuristaLoggato, Set<Materiale>> getMaterialiContest() {
        return materialiContest;
    }

    public void invita(TuristaLoggato turistaLoggato){
        controller.invita(this,turistaLoggato);
    }

    public boolean isOpen() {
        return open;
    }

    public List<TuristaLoggato> getInvitati() {
        return invitati;
    }

}
