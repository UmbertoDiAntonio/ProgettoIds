package ids.unicam.models.contenuti;

import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;
import jakarta.persistence.OneToMany;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Entity
@DiscriminatorValue("ITINERARIO")
public class Itinerario extends Contenuto {
    private String nome=null;
    private final List<PuntoInteresse> percorso = new ArrayList<>();

    public Itinerario() {

    }

    public int getNumeroTappe(){
        return percorso.size();
    }

    public String getNome() {
        return nome;
    }

    @OneToMany
    public List<PuntoInteresse> getPercorso() {
        return percorso;
    }

    public Itinerario(String nome,  PuntoInteresse... puntoInteresse) {
        super();
        this.nome=nome;
        percorso.addAll(Arrays.stream(puntoInteresse).toList());
    }


}

