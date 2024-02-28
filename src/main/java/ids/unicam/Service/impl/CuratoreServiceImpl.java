package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.CuratoreRepository;
import ids.unicam.Service.CuratoreService;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Comune;
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
import org.jetbrains.annotations.Nullable;
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
    private final ContributorServiceImpl contributorService;

    @Autowired
    public CuratoreServiceImpl(CuratoreRepository repository, PoiServiceImpl service, ItinerarioServiceImpl itinerarioServiceImpl,
                               MaterialeServiceImpl materialeServiceImpl, ContestServiceImpl contestServiceImpl,
                               NotificaServiceImpl notificaServiceImpl, ContributorServiceImpl contributorService) {
        this.repository = repository;
        this.poiServiceImpl = service;
        this.itinerarioServiceImpl = itinerarioServiceImpl;
        this.materialeServiceImpl = materialeServiceImpl;
        this.contestServiceImpl = contestServiceImpl;
        this.notificaServiceImpl = notificaServiceImpl;
        this.contributorService = contributorService;
    }


    @Override
    public void deleteById(String id) {
        repository.deleteById(id);
    }

    @Override
    public Optional<Curatore> getById(String username) {
        return repository.findById(username);
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

    @Override
    public List<Curatore> getAll() {
        return repository.findAll();
    }


    private boolean controllaSeInComune(Curatore curatore, Comune comune) {
        return comune.equals(curatore.getComune());
    }


    @Override
    @Transactional
    public PuntoInteresse valutaPuntoInteresse(String usernameCuratore, @NotNull Integer idPuntoInteresse, @Nullable Boolean bStato) throws IllegalArgumentException, UnsupportedOperationException, FuoriComuneException {
        Optional<Curatore> oCuratore = getById(usernameCuratore);
        if (oCuratore.isPresent()) {
            Curatore curatore = oCuratore.get();
            Optional<PuntoInteresse> oPoi = poiServiceImpl.findById(idPuntoInteresse);
            if (oPoi.isPresent()) {
                PuntoInteresse puntoInteresse = oPoi.get();
                if (!curatore.getComune().equals(puntoInteresse.getComune()))
                    throw new FuoriComuneException("curatore non puo' operare fuori dal suo comune");
                if (puntoInteresse.getStato() != Stato.IN_ATTESA)
                    throw new UnsupportedOperationException("punto di interesse già settato");
                Stato stato = Stato.toStatus(bStato);
                if (stato == Stato.IN_ATTESA)
                    throw new UnsupportedOperationException("non puoi impostare stato in attesa");
                puntoInteresse.setStato(stato);
                Notifica notifica = notificaServiceImpl.creaNotifica(curatore, puntoInteresse, stato);
                return poiServiceImpl.save(puntoInteresse);
            } else {
                logger.error("Id del punto di interesse non valido");
                throw new IllegalArgumentException("Id del punto di interesse non valido");
            }
        } else {
            logger.error("username curatore non valido");
            throw new IllegalArgumentException("username curatore non valido");
        }
    }


    @Override
    public MaterialeGenerico valutaMateriale(String usernameCuratore, Integer idMaterialeGenerico, Boolean bStato) throws IllegalArgumentException, UnsupportedOperationException, FuoriComuneException {
        Optional<Curatore> oCuratore = getById(usernameCuratore);
        if (oCuratore.isPresent()) {
            Curatore curatore = oCuratore.get();
            Optional<MaterialeGenerico> oMateriale = materialeServiceImpl.getById(idMaterialeGenerico);
            if (oMateriale.isPresent()) {
                MaterialeGenerico materialeGenerico = oMateriale.get();
                if (false)//TODO
                    throw new FuoriComuneException("curatore non puo' operare fuori dal suo comune");
                if (materialeGenerico.getStato() != Stato.IN_ATTESA)
                    throw new UnsupportedOperationException("materiale già settato");
                Stato stato = Stato.toStatus(bStato);
                if (stato == Stato.IN_ATTESA)
                    throw new UnsupportedOperationException("non puoi impostare stato in attesa");
                materialeGenerico.setStato(stato);
                Notifica notifica = notificaServiceImpl.creaNotifica(curatore, materialeGenerico, stato);
                return materialeServiceImpl.save(materialeGenerico);

            } else {
                logger.error("Id del materiale non valido");
                throw new IllegalArgumentException("Id del materiale non valido");
            }
        } else {
            logger.error("username curatore non valido");
            throw new IllegalArgumentException("username curatore non valido");
        }
    }


    @Override
    public void eliminaPuntoInteresse(String usernameCuratore, Integer idPuntoInteresse) throws IllegalArgumentException, FuoriComuneException {
        Optional<Curatore> oCuratore = getById(usernameCuratore);
        if (oCuratore.isPresent()) {
            Curatore curatore = oCuratore.get();
            Optional<PuntoInteresse> oPoi = poiServiceImpl.getById(idPuntoInteresse);
            if (oPoi.isPresent()) {
                PuntoInteresse puntoInteresse = oPoi.get();
                if (controllaSeInComune(curatore, puntoInteresse.getComune())) {
                    poiServiceImpl.eliminaPuntoInteresse(idPuntoInteresse);
                } else {
                    throw new FuoriComuneException("Il punto di interesse e' fuori dal comune del curatore");
                }
            } else {
                logger.error("Id del punto di interesse non valido");
                throw new IllegalArgumentException("Id del punto di interesse non valido");
            }
        } else {
            logger.error("username curatore non valido");
            throw new IllegalArgumentException("username curatore non valido");
        }
    }

    @Override
    public void eliminaItinerario(String usernameCuratore, Integer idItinerario) throws IllegalArgumentException, FuoriComuneException {
        Optional<Curatore> oCuratore = getById(usernameCuratore);
        if (oCuratore.isPresent()) {
            Curatore curatore = oCuratore.get();
            Optional<Itinerario> oItinerario = itinerarioServiceImpl.getById(idItinerario);
            if (oItinerario.isPresent()) {
                Itinerario itinerario = oItinerario.get();
                if (controllaSeInComune(curatore, itinerario.getComune())) {
                    itinerarioServiceImpl.deleteById(itinerario.getId());
                } else {
                    throw new FuoriComuneException("L'itinerario e' fuori dal comune del curatore");
                }
            } else {
                logger.error("Id dell'itinerario non valido");
                throw new IllegalArgumentException("Id dell'itinerario non valido");
            }
        } else {
            logger.error("username curatore non valido");
            throw new IllegalArgumentException("username curatore non valido");
        }
    }

    @Override
    public void eliminaContest(String usernameCuratore, Integer idContest) throws IllegalArgumentException, FuoriComuneException {
        Optional<Curatore> oCuratore = getById(usernameCuratore);
        if (oCuratore.isPresent()) {
            Curatore curatore = oCuratore.get();
            Optional<Contest> oContest = contestServiceImpl.findById(idContest);
            if (oContest.isPresent()) {
                Contest contest = oContest.get();
                if (controllaSeInComune(curatore, contest.getComune())) {
                    contestServiceImpl.deleteById(contest.getId());
                } else {
                    throw new FuoriComuneException("Il contest e' fuori dal comune del curatore");
                }
            } else {
                logger.error("Id del contest non valido");
                throw new IllegalArgumentException("Id del contest non valido");
            }
        } else {
            logger.error("username curatore non valido");
            throw new IllegalArgumentException("username curatore non valido");
        }
    }

    @Override
    @Transactional
    public void elimina(Curatore curatore, MaterialeGenerico materialeGenerico) throws IllegalArgumentException, FuoriComuneException {
        List<PuntoInteresse> listPuntoInteresse = poiServiceImpl.findActive();
        for (PuntoInteresse puntoInteresse : listPuntoInteresse) {
            if (!puntoInteresse.getComune().equals(curatore.getComune())) {
                throw new FuoriComuneException(curatore.getUsername() + " non può eliminare materiali fuori dal suo comune ");
            }
            puntoInteresse.rimuoviMateriale(materialeGenerico);
            poiServiceImpl.save(puntoInteresse);
        }
        materialeServiceImpl.deleteById(materialeGenerico.getId());
    }

    @Override
    public void condividi(String usernameCuratore, Integer idPunto) throws IllegalArgumentException,UnsupportedOperationException{
        Optional<Curatore> oCuratore = getById(usernameCuratore);
        if (oCuratore.isPresent()) {
            Curatore curatore = oCuratore.get();
            Optional<PuntoInteresse> oPoi = poiServiceImpl.findById(idPunto);
            if (oPoi.isPresent()) {
                PuntoInteresse puntoInteresse = oPoi.get();
                throw new UnsupportedOperationException(puntoInteresse.getId() + "non può ancora essere condiviso da " + curatore);
                //TODO
            } else {
                logger.error("id del punto di interesse non valido");
                throw new IllegalArgumentException("id del punto di interesse non valido");
            }
        } else {
            logger.error("username non valido");
            throw new IllegalArgumentException("username non valido");
        }

    }


    @Transactional
    public void aggiungiOsservatore(String usernameCuratore, String usernameContributorOsservatore) throws IllegalArgumentException {
        Optional<Curatore> oCuratore = getById(usernameCuratore);
        if (oCuratore.isPresent()) {
            Curatore curatore = oCuratore.get();
            Optional<Contributor> oOsservatore = contributorService.getById(usernameContributorOsservatore);
            if (oOsservatore.isPresent()) {
                Contributor osservatore = oOsservatore.get();
                if (curatore.getOsservatori().contains(osservatore)) {
                    logger.warn(osservatore + " stai già seguendo questo curatore");
                    return;
                }
                curatore.getOsservatori().add(osservatore);
                save(curatore);
            } else {
                logger.error("username del contributor non valido");
                throw new IllegalArgumentException("username del contributor non valido");
            }
        } else {
            logger.error("username del curatore non valido");
            throw new IllegalArgumentException("username del curatore non valido");
        }
    }


    @Transactional
    public void rimuoviOsservatore(String usernameCuratore, String usernameContributorOsservatore) throws IllegalArgumentException{
        Optional<Curatore> oCuratore = getById(usernameCuratore);
        if (oCuratore.isPresent()) {
            Curatore curatore = oCuratore.get();
            Optional<Contributor> oOsservatore = contributorService.getById(usernameContributorOsservatore);
            if (oOsservatore.isPresent()) {
                Contributor osservatore = oOsservatore.get();
                curatore.getOsservatori().remove(osservatore);
                save(curatore);
            } else {
                logger.error("username del contributor non valido");
                throw new IllegalArgumentException("username del contributor non valido");
            }
        } else {
            logger.error("username del curatore non valido");
            throw new IllegalArgumentException("username del curatore non valido");
        }
    }

    @Override
    public List<Notifica> getNotifiche(String usernameCurature) throws IllegalArgumentException{
        Optional<Curatore> oCuratore = getById(usernameCurature);
        if (oCuratore.isPresent()) {
            Curatore curatore = oCuratore.get();
            return notificaServiceImpl.getNotifiche(curatore);
        } else {
            logger.error("username del curatore non valido");
            throw new IllegalArgumentException("username del curatore non valido");
        }
    }


    public List<Contributor> getOsservatori(Curatore curatore) {
        return repository.findOsservatoriByCuratore(curatore.getUsername());
    }

    public int getNumeroOsservatori(Curatore curatore) {
        return repository.countNumeroOsservatori(curatore.getUsername());
    }


}

