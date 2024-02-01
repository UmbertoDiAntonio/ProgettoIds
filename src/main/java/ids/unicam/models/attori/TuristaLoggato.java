package ids.unicam.models.attori;

import ids.unicam.Exception.NotInContestException;
import ids.unicam.controller.UtentiController;
import ids.unicam.models.Invito;
import ids.unicam.models.contenuti.*;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Entity
public class TuristaLoggato extends Turista {
    private final List<Contenuto> favourites = new ArrayList<>();
    private final List<Invito> invitiRicevuti = new ArrayList<>();
    private String name=null;
    private String surname=null;
    private Date dateBirthday=null;
    private String password=null;
    private String username=null;
    @Id
    private final String id=UtentiController.generateID();;

    public TuristaLoggato() {

    }

    @OneToMany
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

    @OneToMany
    public List<Contenuto> getFavourites() {
        return favourites;
    }

    public void addFavourites(Contenuto contenuto) {
        if(contenuto.isApproved())
            favourites.add(contenuto);
    }


    protected TuristaLoggato(String name, String surname, Date dateBirthday, String password, String username) {
        this.name = name;
        this.surname = surname;
        this.dateBirthday = dateBirthday;
        this.password = password;
        this.username = username;
        save();
    }

    private void save() {
        //TODO
    }

    public void addPhoto(PuntoInteresse puntoInteresse, Foto foto) {
        puntoInteresse.getMaterialeList().add(foto);
    }

    /**
     * Accetta, se è stato ricevuto un invito a un contest
     *
     * @param invito l'invito da accettare
     */
    public boolean accettaInvito(Invito invito) { //invocato dall'interfaccia
        boolean success = false;
        for (Invito inv : invitiRicevuti) {
            if (inv.equals(invito)) {
                Contest contest = invito.getContest();
                assert contest.getPartecipanti() != null;
                contest.getPartecipanti().add(this);
                success = true;
            }
        }
        return success;
    }


    /**
     * Invia al controller del contest la richiesta di aggiungere un materiale
     *
     * @param contest   il contest in cui aggiungere il materiale
     * @param materiale il materiale da aggiungere
     */
    public void addMaterialeContest(Contest contest, Materiale materiale) {
        if (contest.getPartecipanti().contains(this)) {
            contest.getContestController().aggiungiMateriale(materiale, contest);
        } else {
            throw new NotInContestException("Il Turista dovrebbe prima entrare nel contest, per caricare contenuti su di esso");
        }
    }

    /**
     * Entra nel contest se è aperto
     *
     * @param contest il contest in cui si vuole entrare
     */
    public void joinFreeContest(Contest contest) {
        if (!contest.isOpen()) {
            return;
        }
        contest.getPartecipanti().add(this);
    }

    public boolean logOut() {
        //TODO
        return true;
    }
}
