package ids.unicam.models.contenuti;

import ids.unicam.controller.ContenutoController;
import ids.unicam.models.Tempo;

import java.util.HashSet;
import java.util.Set;

public abstract class Contenuto {

    private Tempo scadenza;

    public void setScadenza(Tempo scadenza) {
        this.scadenza = scadenza;
    }

    public Tempo getScadenza() {
        return scadenza;
    }

    private final long id;
    private boolean approved;

    private final Set<String> tags=new HashSet<>();

    public Contenuto(boolean approved) {
        this.approved = approved;
        this.id = ContenutoController.generateID();
    }

    public long getId() {
        return id;
    }

    public boolean isApproved() {
        return approved;
    }

    public void setApproved(boolean approved) {
        this.approved = approved;
    }

}
