package ids.unicam.models.Service;

import ids.unicam.models.Repository.CuratoreRepository;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.*;
import ids.unicam.utilites.Stato;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class CuratoreService{
    private final CuratoreRepository repository;
    private final PoiService poiService;
    private final ItinerarioService itinerarioService;
    private final MaterialeService materialeService;

    @Autowired
    public CuratoreService(CuratoreRepository repository, PoiService service, ItinerarioService itinerarioService, MaterialeService materialeService) {
        this.repository = repository;
        this.poiService = service;
        this.itinerarioService = itinerarioService;
        this.materialeService = materialeService;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }


    public Curatore save(Curatore curatore) {
        curatore = repository.save(curatore);
        return curatore;
    }


    public Optional<Curatore> findById(int id) {
        return repository.findById(id);
    }


    public List<Curatore> findAll() {
        return repository.findAll();
    }

    public Curatore getLast() {
        return repository.findAll().getLast();
    }

    public Curatore getFirst() {
        return repository.findAll().getFirst();
    }


    public void deleteAll() {
        repository.deleteAll();
    }

    /**
     * Valuta un punto di interesse, in caso di non approvazione lo rimuove dalla lista dei contenuti nel controller del comune associato,
     * notifica i subscriber
     *
     * @param puntoInteresse il punto di interesse che si vuole valutare
     * @param approvato      stato punto di interesse: approvato/non approvato
     */
    public void valuta(@NotNull PuntoInteresse puntoInteresse, Stato approvato) {
        puntoInteresse.setStato(approvato);
        if (approvato == Stato.NOT_APPROVED)
            poiService.deleteById(puntoInteresse.getId());

        //TODO notifica(approvato, puntoInteresse);
    }

    /**
     * Valuta un Materiale,
     * notifica i subscriber
     *
     * @param materialeGenerico il materiale che si vuole valutare
     * @param approvato         approvato o non approvato
     */
    public void valuta(MaterialeGenerico materialeGenerico, Stato approvato) {//TODO
        materialeGenerico.setStato(approvato);
        //TODO notifica(approvato, materialeGenerico);
    }

    public List<Curatore> findByNomeComune(String nomeComune) {
        return repository.findCuratoreByComuneNome(nomeComune);
    }

    public void eliminaContenuto(ContenutoGenerico contenutoGenerico) {
        if (contenutoGenerico instanceof PuntoInteresse puntoInteresse)
            poiService.deleteById(puntoInteresse.getId());
        else if (contenutoGenerico instanceof Itinerario itinerario)
            itinerarioService.deleteById(itinerario.getId());
        else if (contenutoGenerico instanceof Contest contest) {
            //TODO ContestService
        }
    }

    public void eliminaMateriale(MaterialeGenerico materialeGenerico) {
        //TODO devo fare un filtro per quelli del comune del Curatore
        poiService.deleteById(materialeGenerico.getId());
       //TODO  contestService.deleteById(materialeDaEliminare);
    }

    public void rimuoviTappa(Itinerario itinerario,PuntoInteresse tappa){
        itinerarioService.rimuoviTappa(itinerario,tappa);//TODO deve essere un itinerario di sua competenza
    }
}

