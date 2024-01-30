package ids.unicam.models.contenuti;

import ids.unicam.utilites.Punto;

import java.util.ArrayList;
import java.util.List;

public abstract class PuntoInteresse extends Contenuto {
    private final String nome;
    private final Punto pt;
    private final List<Materiale> materialeList = new ArrayList<>();

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
        super(false);
        this.nome = nome;
        this.pt = pt;
    }

    public abstract String mostraDettagli();

    public abstract String getGeneralInfo();



}
