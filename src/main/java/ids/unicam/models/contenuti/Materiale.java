package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaAutenticato;
import jakarta.persistence.*;

import java.util.UUID;

@Entity
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public abstract class Materiale {
    @OneToOne
    private TuristaAutenticato creatore =null;
    private Status stato =Status.NOT_APPROVED;

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

    
    public TuristaAutenticato getCreatore() {
        return creatore;
    }

    public Status getStato() {
        return stato;
    }

    public void setStato(Status stato) {
        this.stato = stato;
    }

    /**
     * Crea un materiale e gli assegna un id random
     * @param creatore l'autore del materiale
     */
    public Materiale(TuristaAutenticato creatore) {
        this.creatore = creatore;
    }


}
