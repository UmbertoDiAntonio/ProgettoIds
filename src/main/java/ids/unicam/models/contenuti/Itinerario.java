package ids.unicam.models.contenuti;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Itinerario extends Contenuto {
    private final String nome;
    private final List<PuntoInteresse> percorso = new ArrayList<>();

    public int getNumeroTappe(){
        return percorso.size();
    }

    public String getNome() {
        return nome;
    }

    public List<PuntoInteresse> getPercorso() {
        return percorso;
    }

    public Itinerario(String nome,  PuntoInteresse... puntoInteresses) {
        super(false);
        this.nome=nome;
        percorso.addAll(Arrays.stream(puntoInteresses).toList());
    }

    public void addTappa(PuntoInteresse puntoInteresse){//TODO siamo sicuri che non vada nel controller?
        percorso.add(puntoInteresse);
    }
    public void addTappa(PuntoInteresse... puntoInteresse){//TODO siamo sicuri che non vada nel controller?
        percorso.addAll(Arrays.stream(puntoInteresse).toList());
    }
    public boolean removeTappa(PuntoInteresse puntoInteresse){//TODO siamo sicuri che non vada nel controller?
        return percorso.remove(puntoInteresse);
    }
}

