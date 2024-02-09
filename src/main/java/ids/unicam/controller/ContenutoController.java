package ids.unicam.controller;

import ids.unicam.models.Service.PoiService;
import ids.unicam.models.contenuti.ContenutoGenerico;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.models.contenuti.PuntoInteresse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Controller
public class ContenutoController {
    private final PoiService poiService;
    private final ArrayList<Itinerario> itinerari = new ArrayList<>();

    @Autowired
    public ContenutoController(PoiService poiService) {
        this.poiService = poiService;
    }

    public List<PuntoInteresse> getContenuti() {
        return poiService.findAll();
    }

    public ArrayList<Itinerario> getItinerari() {
        return itinerari;
    }


    /**
     * Aggiunge un materiale a un punto di interesse
     * @param puntoInteresse il punto di interesse in cui aggiungere il materiale
     * @param materialeGenerico il materiale
     */
    public void aggiungiMateriale(PuntoInteresse puntoInteresse, MaterialeGenerico materialeGenerico) {
        puntoInteresse.getMateriali().add(materialeGenerico);
    }


    /**
     * Aggiunge una nuova tappa a un itinerario esistente
     * @param itinerario l'itinerario a cui aggiungere la tappa
     * @param puntoInteresse il punto di interesse da aggiungere come tappa
     */
    public void aggiungiTappa(Itinerario itinerario, PuntoInteresse puntoInteresse) {
        itinerario.getPercorso().add(puntoInteresse);
    }
    public void aggiungiTappa(Itinerario itinerario, PuntoInteresse... puntoInteresse){
        itinerario.getPercorso().addAll(Arrays.stream(puntoInteresse).toList());
    }
    public void rimuoviTappa(Itinerario itinerario, PuntoInteresse puntoInteresse){
        itinerario.getPercorso().remove(puntoInteresse);
    }

    public void eliminaContenuto(ContenutoGenerico contenutoGenerico) {
        if (contenutoGenerico instanceof PuntoInteresse) {
            poiService.deleteById(contenutoGenerico.getId());
        } else if (contenutoGenerico instanceof Itinerario itinerario) {
            itinerari.remove(itinerario);
        }
    }
}
