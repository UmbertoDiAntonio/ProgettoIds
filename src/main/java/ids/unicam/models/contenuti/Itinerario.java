package ids.unicam.models.contenuti;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Itinerario extends Contenuto {
    private final String nome;
    private final List<Contenuto> percorso = new ArrayList<>();

    public Itinerario(String nome,  PuntoInteresse... puntoInteresses) {
        super(false);
        this.nome=nome;
        percorso.addAll(Arrays.stream(puntoInteresses).toList());
    }

    public void addTappa(PuntoInteresse puntoInteresse){
        percorso.add(puntoInteresse);
    }
    public void addTappa(PuntoInteresse... puntoInteresse){
        percorso.addAll(Arrays.stream(puntoInteresse).toList());
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
