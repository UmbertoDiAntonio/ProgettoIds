package ids.unicam.models.contenuti;

import ids.unicam.Comune;
import jakarta.persistence.Entity;
import jakarta.persistence.OneToMany;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static ids.unicam.Main.logger;


@Entity
public class Itinerario extends ContenutoGenerico {
    private String nome="";
    @OneToMany
    private final List<PuntoInteresse> percorso = new ArrayList<>();

    public Itinerario() {

    }

    public int getNumeroTappe(){
        return percorso.size();
    }

    public String getNome() {
        return nome;
    }


    public List<PuntoInteresse> getPercorso() {
        return percorso;
    }

    public Itinerario(Comune comune, String nome, PuntoInteresse... puntiInteresse) {
        super(comune);
        for (PuntoInteresse puntoInteresse : puntiInteresse) {
            if (!comune.verificaCoordinateComune(puntoInteresse.getPt()) || !puntoInteresse.getStato().asBoolean()) {
                logger.error("Non si possono creare Itinerari con punti non approvati");
                return;
                //TODO
            }
        }
        logger.debug("Creato Itinerario "+nome);
        this.nome=nome;
        percorso.addAll(Arrays.stream(puntiInteresse).toList());
    }
}

