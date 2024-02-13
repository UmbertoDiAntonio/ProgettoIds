package ids.unicam.models.contenuti;

import ids.unicam.Comune;
import ids.unicam.models.Tempo;
import ids.unicam.utilites.Stato;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;


@MappedSuperclass
public abstract class ContenutoGenerico {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE,generator = "sequenza_contenuti")
    @SequenceGenerator(name = "sequenza_contenuti", sequenceName = "PUNTI_DI_INTERESSE_SEQ", allocationSize = 1)
    private int id= 0;

    @OneToOne
    @JoinColumn(name = "nome_comune")
    private Comune comune;
    private Stato stato = Stato.NOT_APPROVED;

    @Transient
    private Tempo scadenza;
    @ElementCollection
    private List<String> tags = new ArrayList<>();

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

    public List<String> getTags() {
        return tags;
    }

    public void setTags(List<String> tags){
        this.tags = tags;
    }

    public void aggiungiTag(String tag) {
        tags.add(tag);
    }

    public ContenutoGenerico(Comune comune) {
        this.comune=comune;
    }
    public ContenutoGenerico(){}

    public Comune getComune() {
        return comune;
    }
}
