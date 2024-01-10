package ids.unicam.models.contenuti;

import ids.unicam.models.Tempo;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.utilites.Punto;

import java.util.ArrayList;
import java.util.List;

public class PuntoInteresse extends Contenuto {
    private String nome;
    private Punto pt;

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
}
