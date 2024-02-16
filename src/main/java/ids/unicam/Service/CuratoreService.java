package ids.unicam.Service;

import ids.unicam.DataBase.Repository.CuratoreRepository;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.*;
import ids.unicam.models.Observer;
import ids.unicam.models.Stato;
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
        return repository.save(curatore);
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
     * @param stato          stato punto di interesse: approvato/non approvato
     */
    @Transactional
    public void valuta(Curatore curatore, @NotNull PuntoInteresse puntoInteresse, Stato stato) {
        puntoInteresse.setStato(stato);
        if (stato == Stato.NOT_APPROVED)
            poiService.deleteById(puntoInteresse.getId());
        //poiService.save(puntoInteresse);
        notifica(stato, curatore, puntoInteresse);
    }

    /**
     * Valuta un Materiale,
     * notifica i subscriber
     *
     * @param materialeGenerico il materiale che si vuole valutare
     * @param stato             approvato o non approvato
     */
    public void valuta(Curatore curatore, MaterialeGenerico materialeGenerico, Stato stato) {
        if (!stato.asBoolean()) {
            materialeService.deleteById(materialeGenerico.getId());
        }
        materialeGenerico.setStato(stato);
        notifica(stato, curatore, materialeGenerico);
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
        if (oPoi.isPresent()) {
            if (!oPoi.get().getComune().equals(curatore.getComune())) {
                logger.error(curatore + " non può eliminare materiali fuori dal suo comune ");
                return;
            }
        }
        materialeService.deleteById(materialeGenerico.getId());
    }

    public void rimuoviTappa(Curatore curatore, Itinerario itinerario, PuntoInteresse tappa) {
        if (!curatore.getComune().equals(itinerario.getComune()))
            logger.warn(curatore + " non può rimuovere tappe da itinerari esterni al suo comune");
        itinerarioService.rimuoviTappa(itinerario, tappa);
    }


    @Transactional
    public void aggiungiOsservatore(Curatore curatore, Contributor osservatore) {
        if(curatore.getOsservatori().contains(osservatore)) {
            logger.error(osservatore+" stai già seguendo questo curatore");
            return;
        }
        curatore.getOsservatori().add(osservatore);
        deleteById(curatore.getId());
        save(curatore);


    }


    @Transactional
    public void rimuoviOsservatore(Curatore curatore, Contributor osservatore) {
        curatore.getOsservatori().remove(osservatore);
        deleteById(curatore.getId());
        save(curatore);
    }

    private void notifica(Stato eventType, Curatore curatore, PuntoInteresse puntoInteresse) {
        for (Observer listener : curatore.getOsservatori()) {
            listener.riceviNotifica(eventType, puntoInteresse);
        }
    }

    private void notifica(Stato eventType, Curatore curatore, MaterialeGenerico materialeGenerico) {
        for (Observer listener : curatore.getOsservatori()) {
            listener.riceviNotifica(eventType, materialeGenerico);
        }
    }

    public List<Contributor> getOsservatori(Curatore curatore) {
        return repository.findOsservatoriByCuratore(curatore.getId());
    }
    public int getNumeroOsservatori(Curatore curatore){
        return repository.countNumeroOsservatori(curatore.getId());
    }
}

