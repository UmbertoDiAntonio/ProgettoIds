package ids.unicam.models.attori;

import ids.unicam.exception.ContestException;
import ids.unicam.models.Invito;
import ids.unicam.models.contenuti.*;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.List;

@Entity
@Table(name = "TURISTI")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@DiscriminatorColumn(name = "TIPO")
@DiscriminatorValue("TuristaAutenticato")
public class TuristaAutenticato extends Turista{
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE,generator = "sequenza_utenti")
    @SequenceGenerator(name = "sequenza_utenti", sequenceName = "users", allocationSize = 1)
    private Integer id;

    private String nome ="";

    private String username="";
    private String cognome ="";
    @Transient
    private GregorianCalendar dataNascita = new GregorianCalendar();
    private String password="";

    @Transient
    private final List<ContenutoGenerico> preferiti = new ArrayList<>();
    @Transient
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

    public Integer getId() {
        return id;
    }

    void setId(Integer id) {
        this.id = id;
    }

    public List<ContenutoGenerico> getPreferiti() {
        return preferiti;
    }

    public void aggiungiPreferito(ContenutoGenerico contenutoGenerico) {
        if(contenutoGenerico.getStato().asBoolean())
            preferiti.add(contenutoGenerico);
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
        puntoInteresse.getMateriali().add(foto);
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
     * @param materialeGenerico il materiale da aggiungere
     */
    public void aggiungiMaterialeAlContest(Contest contest, MaterialeGenerico materialeGenerico) {
        if (contest.getPartecipanti().contains(this)) {
            contest.getContestController().aggiungiMateriale(materialeGenerico, contest);
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
