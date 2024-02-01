package ids.unicam.models.contenuti;

import ids.unicam.utilites.Punto;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;

import java.util.ArrayList;
import java.util.List;

@Entity
public abstract class PuntoInteresse extends Contenuto {
    private String nome=null;
    private Punto pt=null;
    private final List<Materiale> materialeList = new ArrayList<>();

    public PuntoInteresse() {

    }

    @OneToOne
    public Punto getPt() {
        return pt;
    }

    @OneToMany
    public List<Materiale> getMaterialeList() {
        return materialeList;
    }


    public String getNome() {
        return nome;
    }

    public PuntoInteresse(String nome, Punto pt) {
        super();
        this.nome = nome;
        this.pt = pt;
    }

    public abstract String mostraDettagli();

    public abstract String getGeneralInfo();

}
