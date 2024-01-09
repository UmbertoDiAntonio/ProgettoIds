package ids.unicam.models;

import ids.unicam.controller.ContestController;
import ids.unicam.models.attori.TuristaLoggato;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

public class Contest extends Contenuto{
    boolean open;
    ContestController controller;

    public Contest(boolean open, ContestController controller, String obiettivo) {
        this.open = open;
        this.controller = controller;
        this.obiettivo = obiettivo;
    }

    public void setOpen(boolean open) {
        this.open = open;
    }


    private final @Nullable List<TuristaLoggato> invitati=new ArrayList<>();

    String obiettivo;

    //Multimap<String,String> map = ArrayListMultimap.create();
    HashMap<TuristaLoggato, Set<Materiale>> materialiContest = new HashMap<>();

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
