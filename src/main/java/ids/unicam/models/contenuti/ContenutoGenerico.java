package ids.unicam.models.contenuti;

import ids.unicam.Comune;
import ids.unicam.models.Tempo;
import ids.unicam.utilites.Stato;
import jakarta.persistence.*;

import java.util.ArrayList;


@MappedSuperclass
public abstract class ContenutoGenerico {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private int id= 0;

    @OneToOne
    Comune comune;
    private Stato stato = Stato.NOT_APPROVED;

    @Transient
    private Tempo scadenza;
    @ElementCollection
    private final ArrayList<String> tags = new ArrayList<>();

    public void setScadenza(Tempo scadenza) {
        this.scadenza = scadenza;
    }

    public Tempo getScadenza() {
        return scadenza;
    }

    public int getId() {
        return id;
    }

    public Stato getStato() {
        return stato;
    }

    public void setStato(Stato approved) {
        this.stato = approved;
    }

    public ArrayList<String> getTags() {
        return tags;
    }

    public void aggiungiTag(String tag) {
        tags.add(tag);
    }

    public ContenutoGenerico(Comune comune) {
        this.comune=comune;
    }
    public ContenutoGenerico(){}
}
