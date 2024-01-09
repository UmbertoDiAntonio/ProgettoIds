package ids.unicam.models.attori;

import ids.unicam.models.*;

import java.util.ArrayList;
import java.util.List;

public class TuristaLoggato extends Turista {
    private final List<Contenuto> favourites = new ArrayList<>();
    private final List<Invito> invitiRicevuti = new ArrayList<>();

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

}
