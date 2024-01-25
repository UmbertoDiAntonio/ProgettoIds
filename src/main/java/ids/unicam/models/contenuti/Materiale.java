package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaLoggato;


import java.util.UUID;

public class Materiale {

    private final TuristaLoggato author;
    private boolean approved;
    private final UUID id;


    public TuristaLoggato  getAuthor() {
        return author;
    }

    public boolean isApproved() {
        return approved;
    }

    public void setApproved(boolean approved) {
        this.approved = approved;
    }

    public Materiale(boolean approved, TuristaLoggato author) {
        this.approved = approved;
        this.author=author;
        this.id = UUID.randomUUID();
    }

    public UUID getId() {
        return id;
    }
}
