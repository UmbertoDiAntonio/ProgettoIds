package ids.unicam.models.contenuti;

import jakarta.persistence.*;

@Entity
public class Tag {
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE,generator = "sequenza_tag")
    @SequenceGenerator(name = "sequenza_tag", sequenceName = "TAG_SEQ", allocationSize = 1)
    private int id;

    private String valore;

    // Altri attributi della tua entità Tag

    @ManyToOne
    private PuntoInteresse punto;

    public Tag() {

    }
    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }
    public Tag(String valore,PuntoInteresse puntoInteresse){
        this.valore=valore;
        this.punto=puntoInteresse;

    }

    public String getValore() {
        return valore;
    }

    public PuntoInteresse getPunto() {
        return punto;
    }

    @Override
    public String toString() {
        return  valore;
    }
}
