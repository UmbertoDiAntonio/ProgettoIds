package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaLoggato;

import java.util.UUID;

public abstract class Materiale {
    private final TuristaLoggato author;
    private boolean approved=false;
    private final UUID id;
    public UUID getId() {
        return id;
    }

    /**
     *@return  le informazioni sul materiale
     */
    public abstract String get();

    public TuristaLoggato getAuthor() {
        return author;
    }

    public boolean isApproved() {
        return approved;
    }

    public void setApproved(boolean approved) {
        this.approved = approved;
    }

    /**
     * Crea un materiale e gli assegna un id randomico
     * @param author l'autore del materiale
     */
    public Materiale(TuristaLoggato author) {
        this.author=author;
        this.id = UUID.randomUUID();
    }


}
