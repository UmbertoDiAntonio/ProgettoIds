package ids.unicam.models.contenuti.materiali;

import ids.unicam.models.DTO.MaterialeDTO;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Stato;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;


@Getter
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

    @Setter
    private Stato stato = Stato.IN_ATTESA;

    private String file;

    public MaterialeGenerico() {
    }

    /**
     *@return  le informazioni sul materiale
     */
    public abstract String getBase64();

    public MaterialeGenerico(MaterialeDTO materialeDTO) {
        this.file ="./src/main/resources/materials/"+materialeDTO.getPathFile();
        this.creatore = materialeDTO.getCreatore();
    }

    @Override
    public String toString() {
        return "MaterialeGenerico{" +
                "id=" + id +
                ", creatore=" + creatore +
                ", stato=" + stato +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        MaterialeGenerico that = (MaterialeGenerico) o;

        if (id != that.id) return false;
        return creatore.equals(that.creatore);
    }

    @Override
    public int hashCode() {
        int result = id;
        result = 31 * result + creatore.hashCode();
        return result;
    }
}
