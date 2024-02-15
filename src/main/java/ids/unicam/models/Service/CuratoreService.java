package ids.unicam.models.Service;

import ids.unicam.models.Repository.CuratoreRepository;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.*;
import ids.unicam.utilites.Stato;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class CuratoreService {
    private final CuratoreRepository repository;
    private final PoiService poiService;
    private final ItinerarioService itinerarioService;
    private final MaterialeService materialeService;
    private final ContestService contestService;

    @Autowired
    public CuratoreService(CuratoreRepository repository, PoiService service, ItinerarioService itinerarioService, MaterialeService materialeService, ContestService contestService) {
        this.repository = repository;
        this.poiService = service;
        this.itinerarioService = itinerarioService;
        this.materialeService = materialeService;
        this.contestService = contestService;
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
    @Transactional
    public void valuta(@NotNull PuntoInteresse puntoInteresse, Stato approvato) {
        puntoInteresse.setStato(approvato);
        if (approvato == Stato.NOT_APPROVED)
            poiService.deleteById(puntoInteresse.getId());
        //poiService.save(puntoInteresse);
        //TODO notifica(approvato, puntoInteresse);
    }

    /**
     * Valuta un Materiale,
     * notifica i subscriber
     *
     * @param materialeGenerico il materiale che si vuole valutare
     * @param stato         approvato o non approvato
     */
    public void valuta(MaterialeGenerico materialeGenerico, Stato stato) {
        if(!stato.asBoolean()){
            materialeService.deleteById(materialeGenerico.getId());
        }
        materialeGenerico.setStato(stato);
        //TODO notifica(approvato, materialeGenerico);
    }

    public List<Curatore> findByNomeComune(String nomeComune) {
        return repository.findCuratoreByComuneNome(nomeComune);
    }

    public void elimina(PuntoInteresse puntoInteresse) {
        poiService.eliminaPuntoInteresse(puntoInteresse.getId());
    }

    public void elimina(Itinerario itinerario) {
        itinerarioService.deleteById(itinerario.getId());
    }

    public void elimina(Contest contest) {
        contestService.deleteById(contest.getId());
    }
    public void condividi(ContenutoGenerico contenutoGenerico) {
        throw new UnsupportedOperationException(contenutoGenerico.getId() + "non può ancora essere condiviso");
        //TODO
    }


    public void elimina(Curatore curatore, MaterialeGenerico materialeGenerico) {
        Optional<PuntoInteresse> oPoi = poiService.findById(materialeGenerico.getIdProprietario());
        if(oPoi.isPresent()){
            if(!oPoi.get().getComune().equals(curatore.getComune())){
                logger.error(curatore+" non può eliminare materiali fuori dal suo comune ");
                return;
            }
        }
        materialeService.deleteById(materialeGenerico.getId());
    }

    public void rimuoviTappa(Curatore curatore,Itinerario itinerario, PuntoInteresse tappa) {
        if(!curatore.getComune().equals(itinerario.getComune()))
            logger.warn(curatore + " non può rimuovere tappe da itinerari esterni al suo comune");
        itinerarioService.rimuoviTappa(itinerario, tappa);
    }
}

