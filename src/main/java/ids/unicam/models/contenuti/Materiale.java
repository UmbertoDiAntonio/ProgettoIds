package ids.unicam.models.contenuti;

import ids.unicam.models.attori.Contributor;

public class Materiale {
    private final PuntoInteresse owner;
    private final Contributor author;
    private boolean approved;

    public PuntoInteresse getOwner() {
        return owner;
    }

    public Contributor getAuthor() {
        return author;
    }

    public boolean isApproved() {
        return approved;
    }

    public void setApproved(boolean approved) {
        this.approved = approved;
    }

    public Materiale(boolean approved, PuntoInteresse owner, Contributor author) {
        this.approved = approved;
        this.author=author;
        this.owner=owner;
        owner.getMaterialeList().add(this);
    }
}
