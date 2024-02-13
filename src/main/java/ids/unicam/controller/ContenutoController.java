package ids.unicam.controller;

import ids.unicam.models.Service.MaterialeService;
import ids.unicam.models.Service.PoiService;
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
    private final MaterialeService materialeService;

    @Autowired
    public ContenutoController(PoiService poiService, MaterialeService materialeService) {
        this.poiService = poiService;
        this.materialeService = materialeService;
    }

    public List<PuntoInteresse> getContenuti() {
        return poiService.findAll();
    }

    public ArrayList<Itinerario> getItinerari() {
        return itinerari;
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




}
