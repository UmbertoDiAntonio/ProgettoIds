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

    private long id;
    private boolean approved = false;

    private final Set<String> tags=new HashSet<>();

    private boolean isContest;
    public long getId() {
        return id;
    }

    public boolean isContest() {
        return isContest;
    }

    public boolean isApproved() {
        return approved;
    }

    public void setApproved(boolean approved) {
        this.approved = approved;
    }

    public Contenuto(boolean approved) {
        this.approved = approved;
        this.id = ContenutoController.generateID();
    }
}
