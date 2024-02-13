package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.utilites.Stato;
import jakarta.persistence.*;

@Entity
@Table(name = "Materiali")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(name="tipo")
public abstract class MaterialeGenerico {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE,generator = "sequenza_materiali")
    @SequenceGenerator(name = "sequenza_materiali", sequenceName = "MATERIALE_SEQ", allocationSize = 1)
    private int id= 0;
    @OneToOne
    private TuristaAutenticato creatore =null;
    private Stato stato = Stato.NOT_APPROVED;

    @JoinColumn(name = "idProprietario")
    public int idProprietario;
    public MaterialeGenerico() {

    }

    public int getId() {
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
    public MaterialeGenerico(TuristaAutenticato creatore,PuntoInteresse puntoInteresse) {
        this.idProprietario=puntoInteresse.getId();
        this.creatore = creatore;
    }
    public MaterialeGenerico(TuristaAutenticato creatore,Contest contest) {
        this.idProprietario=contest.getId();
        this.creatore = creatore;
    }


}
