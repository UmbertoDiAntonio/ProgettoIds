package ids.unicam.models.contenuti;

import ids.unicam.controller.ContenutoController;
import ids.unicam.models.Tempo;
import jakarta.persistence.*;

import java.util.ArrayList;


@Entity
public abstract class Contenuto {

    @Id
    private final long id;
    private boolean stato = false;

    @Transient
    private Tempo scadenza;
    @ElementCollection
    private final ArrayList<String> tags=new ArrayList<>();

    public void setScadenza(Tempo scadenza) {
        this.scadenza = scadenza;
    }

    public Tempo getScadenza() {
        return scadenza;
    }
    public long getId() {
        return id;
    }

    public boolean getStato() {
        return stato;
    }

    public void setStato(boolean approved) {
        this.stato = approved;
    }

    public ArrayList<String> getTags() {
        return tags;
    }
    public void aggiungiTag(String tag){
        tags.add(tag);
    }
    public Contenuto() {
        this.id = ContenutoController.generaID();
    }
}
