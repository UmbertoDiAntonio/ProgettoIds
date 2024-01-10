package ids.unicam.models.contenuti;

public abstract class Materiale {
    private PuntoInteresse owner;
    private String author;
    private boolean pending;

    public PuntoInteresse getOwner() {
        return owner;
    }

    public String getAuthor() {
        return author;
    }

    public boolean isPending() {
        return pending;
    }

    public void setPending(boolean pending) {
        this.pending = pending;
    }
}
