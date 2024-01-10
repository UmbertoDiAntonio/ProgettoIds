package ids.unicam.models.contenuti;

import java.util.ArrayList;
import java.util.List;

public class Itinerario extends Contenuto {
    private List<Contenuto> percorso = new ArrayList<>();

    public void addTappa(PuntoInteresse puntoInteresse){
        percorso.add(puntoInteresse);
    }

    public List<Contenuto> getPercorso() {
        return percorso;
    }
}
