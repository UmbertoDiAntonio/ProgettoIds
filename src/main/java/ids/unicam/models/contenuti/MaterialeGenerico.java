package ids.unicam.models.contenuti;

import ids.unicam.exception.ContestException;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.utilites.Stato;
import jakarta.persistence.*;

import static ids.unicam.Main.logger;

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
    private int idProprietario;
    public MaterialeGenerico() {

    }

    public int getIdProprietario() {
        return idProprietario;
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
        if(!contest.getPartecipanti().contains(creatore)) {
            logger.error("Devi essere iscritto al contest per caricare materiale su di esso");
            throw new ContestException("Devi essere iscritto al contest per caricare materiale su di esso");
        }
        this.idProprietario=contest.getId();
        this.creatore = creatore;
    }


}
