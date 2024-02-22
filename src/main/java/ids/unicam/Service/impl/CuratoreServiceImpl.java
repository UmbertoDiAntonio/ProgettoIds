package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.CuratoreRepository;
import ids.unicam.Service.CuratoreService;
import ids.unicam.models.Comune;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class CuratoreServiceImpl implements CuratoreService {
    private final CuratoreRepository repository;
    private final PoiServiceImpl poiServiceImpl;
    private final ItinerarioServiceImpl itinerarioServiceImpl;
    private final MaterialeServiceImpl materialeServiceImpl;
    private final ContestServiceImpl contestServiceImpl;
    private final NotificaServiceImpl notificaServiceImpl;

    @Autowired
    public CuratoreServiceImpl(CuratoreRepository repository, PoiServiceImpl service, ItinerarioServiceImpl itinerarioServiceImpl,
                               MaterialeServiceImpl materialeServiceImpl, ContestServiceImpl contestServiceImpl,
                               NotificaServiceImpl notificaServiceImpl) {
        this.repository = repository;
        this.poiServiceImpl = service;
        this.itinerarioServiceImpl = itinerarioServiceImpl;
        this.materialeServiceImpl = materialeServiceImpl;
        this.contestServiceImpl = contestServiceImpl;
        this.notificaServiceImpl = notificaServiceImpl;
    }


    @Override
    public void deleteById(String id) {
        repository.deleteById(id);
    }

    @Override
    public Optional<Curatore> getById(String username) {
        return repository.findById(username);
    }

    @Override
    public Curatore update(RichiestaCreazioneContributorDTO contributorDTO, String username) {
        //TODO
        return null;
    }


    public Curatore save(Curatore curatore) {
        return repository.save(curatore);
    }

    public void deleteAll() {
        repository.deleteAll();
    }

    public List<Curatore> findByNomeComune(String nomeComune) {
        return repository.findCuratoreByComuneNome(nomeComune);
    }

    /**
     * Valuta un punto di interesse, in caso di non approvazione lo rimuove dalla lista dei contenuti nel controller del comune associato,
     * notifica i subscriber
     *
     * @param puntoInteresse il punto di interesse che si vuole valutare
     * @param stato          stato punto di interesse: approvato/non approvato
     */
    @Override
    @Transactional
    public void valuta(Curatore curatore, @NotNull PuntoInteresse puntoInteresse, Stato stato) {
        if (!curatore.getComune().equals(puntoInteresse.getComune()))
            throw new UnsupportedOperationException("curatore non puo' operare fuori dal suo comune");
        if(puntoInteresse.getStato() != Stato.IN_ATTESA)
            throw new UnsupportedOperationException("punto di interesse già settato");
        if(stato==Stato.IN_ATTESA)
            throw new UnsupportedOperationException("stato in attesa");
        puntoInteresse.setStato(stato);
        poiServiceImpl.save(puntoInteresse);
        Notifica notifica = notificaServiceImpl.creaNotifica(curatore, puntoInteresse, stato);
        System.out.println(notifica);
    }

    /**
     * Valuta un Materiale,
     * notifica i subscriber
     *
     * @param materialeGenerico il materiale che si vuole valutare
     * @param stato             approvato o non approvato
     */
    @Override
    public void valuta(Curatore curatore, MaterialeGenerico materialeGenerico, Stato stato) {
        materialeGenerico.setStato(stato);
        if (stato == Stato.NON_APPROVATO)
            materialeServiceImpl.deleteById(materialeGenerico.getId());
        else materialeServiceImpl.save(materialeGenerico);

        Notifica notifica = notificaServiceImpl.creaNotifica(curatore, materialeGenerico, stato);
    }

    private boolean controllaSeInComune(Curatore curatore, Comune comune) {
        return comune.equals(curatore.getComune());
    }

    @Override
    public void elimina(Curatore curatore, PuntoInteresse puntoInteresse) {
        if (controllaSeInComune(curatore, puntoInteresse.getComune())) {
            poiServiceImpl.eliminaPuntoInteresse(puntoInteresse.getId());
        }
    }


    @Override
    public void elimina(Curatore curatore, Itinerario itinerario) {
        if (controllaSeInComune(curatore, itinerario.getComune())) {
            itinerarioServiceImpl.deleteById(itinerario.getId());
        }
    }

    @Override
    public void elimina(Curatore curatore, Contest contest) {
        if (controllaSeInComune(curatore, contest.getComune())) {
            contestServiceImpl.deleteById(contest.getId());
        }
    }

    @Override
    @Transactional
    public void elimina(Curatore curatore, MaterialeGenerico materialeGenerico) {
        List<PuntoInteresse> listPuntoInteresse = poiServiceImpl.findActive();
        for (PuntoInteresse puntoInteresse : listPuntoInteresse) {
            if (!puntoInteresse.getComune().equals(curatore.getComune())) {
                logger.error(curatore + " non può eliminare materiali fuori dal suo comune ");
                return;
            }
            puntoInteresse.rimuoviMateriale(materialeGenerico);
            poiServiceImpl.save(puntoInteresse);
        }
        materialeServiceImpl.deleteById(materialeGenerico.getId());
    }

    @Override
    public void condividi(Curatore curatore, PuntoInteresse contenutoGenerico) {
        throw new UnsupportedOperationException(contenutoGenerico.getId() + "non può ancora essere condiviso da " + curatore);
        //TODO
    }


    @Override
    public void rimuoviTappa(Curatore curatore, Itinerario itinerario, PuntoInteresse tappa) {
        if (!curatore.getComune().equals(itinerario.getComune()))
            logger.warn(curatore + " non può rimuovere tappe da itinerari esterni al suo comune");
        itinerarioServiceImpl.rimuoviTappa(itinerario, tappa);
    }

    @Override
    public List<Curatore> getAll() {
        return repository.findAll();
    }


    @Transactional
    public void aggiungiOsservatore(Curatore curatore, Contributor osservatore) {
        if (curatore.getOsservatori().contains(osservatore)) {
            logger.error(osservatore + " stai già seguendo questo curatore");
            return;
        }
        curatore.getOsservatori().add(osservatore);
        deleteById(curatore.getUsername());
        save(curatore);
    }


    @Transactional
    public void rimuoviOsservatore(Curatore curatore, Contributor osservatore) {
        curatore.getOsservatori().remove(osservatore);
        deleteById(curatore.getUsername());
        save(curatore);
    }

    public List<Contributor> getOsservatori(Curatore curatore) {
        return repository.findOsservatoriByCuratore(curatore.getUsername());
    }

    public int getNumeroOsservatori(Curatore curatore) {
        return repository.countNumeroOsservatori(curatore.getUsername());
    }

}

