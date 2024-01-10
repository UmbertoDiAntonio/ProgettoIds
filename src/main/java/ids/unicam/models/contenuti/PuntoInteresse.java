package ids.unicam.models.contenuti;

import ids.unicam.models.Tempo;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.utilites.Punto;

import java.util.ArrayList;
import java.util.List;

public class PuntoInteresse extends Contenuto {
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

    public PuntoInteresse(Punto pt, Tempo scadenza) {
        this.pt = pt;
        this.scadenza = scadenza;
    }
}
