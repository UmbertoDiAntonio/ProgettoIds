package ids.unicam.models.attori;

import ids.unicam.controller.UtentiController;
import ids.unicam.models.Invito;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Foto;
import ids.unicam.models.contenuti.PuntoInteresse;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class TuristaLoggato extends Turista {
    private final List<Contenuto> favourites = new ArrayList<>();
    private final List<Invito> invitiRicevuti = new ArrayList<>();
    private String name;
    private String surname;
    private Date dateBirthday;
    private String password; //TODO
    private String username;
    private String id;

    public List<Invito> getInvitiRicevuti() {
        return invitiRicevuti;
    }

    public String getName() {
        return name;
    }

    public String getSurname() {
        return surname;
    }

    public Date getDateBirthday() {
        return dateBirthday;
    }

    public String getPassword() {
        return password;
    }

    public String getUsername() {
        return username;
    }

    public String getId() {
        return id;
    }

    public TuristaLoggato(String name, String surname, Date dateBirthday, String password, String username) {
        this.name = name;
        this.surname = surname;
        this.dateBirthday = dateBirthday;
        this.password = password;
        this.username = username;
        this.id = UtentiController.generateID();
        save();
    }

    private void save(){
        //TODO
    }

    public List<Contenuto> getFavourites() {
        return favourites;
    }

    public void addFavourites(Contenuto contenuto) {
        favourites.add(contenuto);
    }

    public void addPhoto(PuntoInteresse puntoInteresse, Foto foto) {
        //TODO
    }

    public void joinContest(Invito invito) { //invocato dall'interfaccia
        for (Invito inv : invitiRicevuti) {
            if (inv.equals(invito)) {
                Contest contest = invito.getContest();
                if (contest.isOpen()) {
                    //TODO
                    return;
                }
                assert contest.getInvitati() != null;
                contest.getInvitati().add(this);
            }
        }
    }

    public boolean logOut() {
        //TODO
        return true;
    }
}
