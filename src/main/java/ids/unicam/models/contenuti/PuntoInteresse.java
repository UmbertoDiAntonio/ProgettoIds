package ids.unicam.models.contenuti;

import ids.unicam.utilites.Punto;
import jakarta.persistence.Entity;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Transient;

import java.util.ArrayList;
import java.util.List;


@Entity
public abstract class PuntoInteresse extends Contenuto {

    private String nome="";

    @OneToOne
    private Punto pt=new Punto(0,0);

    @OneToMany
    private final List<Materiale> materialeList = new ArrayList<>();

    public PuntoInteresse() {

    }

    public Punto getPt() {
        return pt;
    }

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
