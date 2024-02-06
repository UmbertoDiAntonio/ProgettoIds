package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.utilites.Stato;
import jakarta.persistence.*;

import java.util.UUID;

@Entity
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public abstract class MaterialeGenerico {
    @OneToOne
    private TuristaAutenticato creatore =null;
    private Stato stato = Stato.NOT_APPROVED;

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private long id= 0;

    public MaterialeGenerico() {

    }

    public long getId() {
        return id;
    }

    /**
     *@return  le informazioni sul materiale
     */
    public abstract String get();

    
    public TuristaAutenticato getCreatore() {
        return creatore;
    }

    public Stato getStato() {
        return stato;
    }

    public void setStato(Stato stato) {
        this.stato = stato;
    }

    /**
     * Crea un materiale e gli assegna un id random
     * @param creatore l'autore del materiale
     */
    public MaterialeGenerico(TuristaAutenticato creatore) {
        this.creatore = creatore;
    }


}
