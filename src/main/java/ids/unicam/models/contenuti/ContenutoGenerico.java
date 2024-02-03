package ids.unicam.models.contenuti;

import ids.unicam.controller.ContenutoController;
import ids.unicam.models.Tempo;
import ids.unicam.utilites.Stato;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Transient;

import java.util.ArrayList;


@Entity
public abstract class ContenutoGenerico {

    @Id
    private final long id;
    private Stato stato = Stato.NOT_APPROVED;

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

    public Stato getStato() {
        return stato;
    }

    public void setStato(Stato approved) {
        this.stato = approved;
    }

    public ArrayList<String> getTags() {
        return tags;
    }
    public void aggiungiTag(String tag){
        tags.add(tag);
    }
    public ContenutoGenerico() {
        this.id = ContenutoController.generaID();
    }
}
