package ids.unicam.models.contenuti;

import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.TuristaLoggato;

public class Materiale {
    private final PuntoInteresse owner;

    private final TuristaLoggato author;
    private boolean approved;

    public PuntoInteresse getOwner() {
        return owner;
    }

    public TuristaLoggato getAuthor() {
        return author;
    }

    public boolean isApproved() {
        return approved;
    }

    public void setApproved(boolean approved) {
        this.approved = approved;
    }

    public Materiale(boolean approved, PuntoInteresse owner, TuristaLoggato author) {
        this.approved = approved;
        this.author=author;
        this.owner=owner;
        owner.getMaterialeList().add(this);
    }

}
