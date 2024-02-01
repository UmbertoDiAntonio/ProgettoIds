package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaLoggato;
import jakarta.persistence.*;

import java.util.UUID;

@Entity
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public abstract class Materiale {
    @OneToOne
    private TuristaLoggato author=null;
    private boolean approved=false;

    @Id
    private final UUID id=UUID.randomUUID();

    public Materiale() {

    }

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
     * Crea un materiale e gli assegna un id random
     * @param author l'autore del materiale
     */
    public Materiale(TuristaLoggato author) {
        this.author=author;
    }


}
