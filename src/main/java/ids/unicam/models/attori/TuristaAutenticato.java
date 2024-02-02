package ids.unicam.models.attori;

import ids.unicam.Exception.ContestException;
import ids.unicam.models.Invito;
import ids.unicam.models.contenuti.*;
import jakarta.persistence.*;


import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.List;

@Entity
public class TuristaAutenticato extends Turista{
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private long id= 0;
    private String nome ="";
    private String username="";
    private String cognome ="";
    private GregorianCalendar dataNascita = new GregorianCalendar();
    private String password="";

    @OneToMany
    private final List<Contenuto> preferiti = new ArrayList<>();
    @OneToMany
    private final List<Invito> invitiRicevuti = new ArrayList<>();


    public TuristaAutenticato() {

    }


    public List<Invito> getInvitiRicevuti() {
        return invitiRicevuti;
    }

    public String getNome() {
        return nome;
    }

    public String getCognome() {
        return cognome;
    }

    public GregorianCalendar getDataNascita() {
        return dataNascita;
    }

    public String getPassword() {
        return password;
    }

    public String getUsername() {
        return username;
    }

    public long getId() {
        return id;
    }


    public List<Contenuto> getPreferiti() {
        return preferiti;
    }

    public void aggiungiPreferito(Contenuto contenuto) {
        if(contenuto.getStato())
            preferiti.add(contenuto);
    }


    protected TuristaAutenticato(String nome, String cognome, GregorianCalendar dataNascita, String password, String username) {
        this.nome = nome;
        this.cognome = cognome;
        this.dataNascita = dataNascita;
        this.password = password;
        this.username = username;
        save();
    }

    private void save() {
        //TODO
    }

    public void aggiungiFoto(PuntoInteresse puntoInteresse, Foto foto) {
        puntoInteresse.getListaMateriali().add(foto);
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
    public void aggiungiMaterialeAlContest(Contest contest, Materiale materiale) {
        if (contest.getPartecipanti().contains(this)) {
            contest.getContestController().aggiungiMateriale(materiale, contest);
        } else {
            throw new ContestException("Il Turista dovrebbe prima entrare nel contest, per caricare contenuti su di esso");
        }
    }

    /**
     * Entra nel contest se è aperto
     *
     * @param contest il contest in cui si vuole entrare
     */
    public void partecipaAlContest(Contest contest) {
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
