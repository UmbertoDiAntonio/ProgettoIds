package ids.unicam.models.contenuti;

import java.util.ArrayList;
import java.util.List;

public class Itinerario extends Contenuto {
    private final String nome;
    private final List<Contenuto> percorso = new ArrayList<>();

    public Itinerario(String nome, boolean approved) {
        super(approved);
        this.nome=nome;
    }

    public void addTappa(PuntoInteresse puntoInteresse){
        percorso.add(puntoInteresse);
    }
    public boolean removeTappa(PuntoInteresse puntoInteresse){
        //TODO
        return true;
    }

    public String getNome() {
        return nome;
    }

    public List<Contenuto> getPercorso() {
        return percorso;
    }
}
