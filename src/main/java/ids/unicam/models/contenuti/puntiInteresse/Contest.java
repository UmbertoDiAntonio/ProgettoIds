package ids.unicam.models.contenuti.puntiInteresse;

import ids.unicam.models.contenuti.Expirable;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.users.TuristaAutenticato;
import ids.unicam.models.users.organizzazioneComune.Animatore;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;

@Entity
public class Contest extends PuntoInteresse implements Taggable, Expirable {

    @Column(name = "Aperto")
    private boolean open;
    private String obiettivo="";
    @OneToOne
    private Animatore creatore =null;

    @OneToMany(fetch = FetchType.EAGER)
    private final List<TuristaAutenticato> partecipanti = new ArrayList<>();

    private String nomeContest =null;

    public List<TuristaAutenticato> getPartecipanti() {
        return partecipanti;
    }

    public Contest() {
    }

    public Animatore getCreatore() {
        return creatore;
    }

    public String getNomeContest() {
        return nomeContest;
    }

    public void setOpen(boolean open) {
        this.open = open;
    }

    public String getObiettivo() {
        return obiettivo;
    }

    public boolean isOpen() {
        return open;
    }

    public Contest(String nomeContest, boolean open, String obiettivo, Animatore creatore) {
        super(creatore.getComune(), nomeContest, creatore.getComune().getPosizione(), TipologiaPuntoInteresse.CONTEST);
        this.open = open;
        this.obiettivo = obiettivo;
        this.creatore = creatore;
        this.nomeContest = nomeContest;
    }





}
