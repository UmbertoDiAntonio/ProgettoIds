package ids.unicam.models.contenuti;

import ids.unicam.controller.ContenutoController;
import ids.unicam.models.Tempo;
import jakarta.persistence.*;
import jdk.jfr.Enabled;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;



public abstract class Contenuto {


    private final long id;
    private boolean approved = false;


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
    public Contenuto() {
        this.id = ContenutoController.generateID();
    }
}
