package ids.unicam.models.attori;

import ids.unicam.controller.TuristaController;
import ids.unicam.models.*;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Foto;
import ids.unicam.models.contenuti.PuntoInteresse;

import java.util.ArrayList;
import java.util.List;

public class TuristaLoggato extends Turista {
    private final List<Contenuto> favourites = new ArrayList<>();
    private final List<Invito> invitiRicevuti = new ArrayList<>();

    private long id=0;

    public long getId() {
        return id;
    }

    public TuristaLoggato() {
        this.id= TuristaController.generateID();
    }

    public List<Contenuto> getFavourites() {
        return favourites;
    }

    public void addFavourites(Contenuto contenuto) {
        favourites.add(contenuto);
    }

    public void addPhoto(PuntoInteresse puntoInteresse, Foto foto) {

    }

    public void joinContest(Invito invito) { //invocato dall'interfaccia
        for (Invito inv : invitiRicevuti) {
            if (inv.equals(invito)){
                Contest contest= invito.getContest();
                if(contest.isOpen()){
                    //TODO
                    return;
                }
                assert contest.getInvitati() != null;
                contest.getInvitati().add(this);
            } ;
        }
    }

    public boolean logOut(){
        //TODO
        return true;
    }
}
