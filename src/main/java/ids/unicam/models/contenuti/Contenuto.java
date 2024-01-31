package ids.unicam.models.contenuti;

import ids.unicam.controller.ContenutoController;
import ids.unicam.models.Tempo;

import java.util.ArrayList;
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

    private final ArrayList<String> tags=new ArrayList<>();


    public long getId() {
        return id;
    }

    public boolean isApproved() {
        return approved;
    }

    public void setApproved(boolean approved) {
        this.approved = approved;
    }

    public ArrayList<String> getTags() {
        return tags;
    }
    public void addTag(String tag){
        tags.add(tag);
    }
    public Contenuto(boolean approved) {
        this.approved = approved;
        this.id = ContenutoController.generateID();
    }
}
