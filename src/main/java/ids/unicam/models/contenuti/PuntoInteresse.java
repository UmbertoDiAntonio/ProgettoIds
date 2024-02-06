package ids.unicam.models.contenuti;

import ids.unicam.utilites.Punto;
import jakarta.persistence.Entity;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;

import java.util.ArrayList;
import java.util.List;

import static ids.unicam.Main.logger;


@Entity
public abstract class PuntoInteresse extends ContenutoGenerico {

    private String nome="";

    @OneToOne
    private Punto pt=new Punto(0,0);

    @OneToMany
    private final List<MaterialeGenerico> listaMateriali = new ArrayList<>();

    public PuntoInteresse() {

    }

    public Punto getPt() {
        return pt;
    }

    public List<MaterialeGenerico> getMateriali() {
        return listaMateriali;
    }


    public String getNome() {
        return nome;
    }

    public PuntoInteresse(String nome, Punto pt) {
        logger.debug("Creato POI "+nome+" in "+pt);
        this.nome = nome;
        this.pt = pt;
    }

    public abstract String mostraInformazioniDettagliate();

    public abstract String mostraInformazioniGeneriche();



}
