package ids.unicam.models.contenuti;

import ids.unicam.utilites.Punto;

import java.util.ArrayList;
import java.util.List;

public abstract class PuntoInteresse extends Contenuto {
    private final String nome;
    private final Punto pt;


    public Punto getPt() {
        return pt;
    }
    List<Materiale> materialeList =new ArrayList<>();
    List<Materiale> waitingMaterials =new ArrayList<>();

    public List<Materiale> getMaterialeList() {
        return materialeList;
    }
    public void addMateriale(Materiale materiale){
        materialeList.add(materiale);
    }

    public String getNome() {
        return nome;
    }

    public PuntoInteresse(String nome, Punto pt) {
        super(false);
        this.nome=nome;
        this.pt = pt;
    }

    public abstract void mostraDettagli();

    public abstract void getGeneralInfo();
}
