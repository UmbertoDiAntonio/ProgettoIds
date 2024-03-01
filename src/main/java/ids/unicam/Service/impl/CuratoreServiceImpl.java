package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.CuratoreRepository;
import ids.unicam.Service.*;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Comune;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class CuratoreServiceImpl implements CuratoreService {
    private final CuratoreRepository repository;
    private final PoiService poiServiceImpl;
    private final ItinerarioService itinerarioServiceImpl;
    private final MaterialeService materialeServiceImpl;
    private final ContestService contestServiceImpl;
    private final NotificaService notificaService;

    @Autowired
    public CuratoreServiceImpl(CuratoreRepository repository, PoiService poiServiceImpl, ItinerarioService itinerarioServiceImpl, MaterialeService materialeServiceImpl, ContestService contestServiceImpl, NotificaService notificaService) {
        this.repository = repository;
        this.poiServiceImpl = poiServiceImpl;
        this.itinerarioServiceImpl = itinerarioServiceImpl;
        this.materialeServiceImpl = materialeServiceImpl;
        this.contestServiceImpl = contestServiceImpl;
        this.notificaService = notificaService;
    }


    @Override
    public void deleteByUsername(String id) {
        repository.deleteById(id);
    }

    @Override
    public Optional<Curatore> getByUsername(String username) {
        return repository.findById(username);
    }

    @Override
    public Curatore save(Curatore curatore) {
        return repository.save(curatore);
    }


    @Override
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
    public PuntoInteresse valutaPuntoInteresse(String usernameCuratore,int idPuntoInteresse, @Nullable Boolean bStato) throws IllegalArgumentException, UnsupportedOperationException, FuoriComuneException {
        Optional<Curatore> oCuratore = getByUsername(usernameCuratore);
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
                notificaService.creaNotificaApprovazione(curatore, puntoInteresse, stato);
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
    public MaterialeGenerico valutaMateriale(String usernameCuratore, int idMaterialeGenerico, Boolean bStato) throws IllegalArgumentException, UnsupportedOperationException, FuoriComuneException {
        Optional<Curatore> oCuratore = getByUsername(usernameCuratore);
        if (oCuratore.isPresent()) {
            Curatore curatore = oCuratore.get();
            Optional<MaterialeGenerico> oMateriale = materialeServiceImpl.getById(idMaterialeGenerico);
            if (oMateriale.isPresent()) {
                MaterialeGenerico materialeGenerico = oMateriale.get();
                Optional<PuntoInteresse> oPoi = poiServiceImpl.getPoiContainingMaterial(materialeGenerico);
                if (oPoi.isEmpty()) {
                    Optional<Contest> oContest = contestServiceImpl.getContestContainingMaterial(materialeGenerico);
                    if (oContest.isPresent()) {
                        if (!oContest.get().getComune().equals(curatore.getComune()))
                            throw new FuoriComuneException("curatore non puo' operare fuori dal suo comune");
                    } else {
                        logger.error("il materiale non è caricato ne' su un contest ne' su un punto di interesse");
                        throw new IllegalArgumentException("il materiale non è caricato ne' su un contest ne' su un punto di interesse");
                    }
                } else {
                    if (!oPoi.get().getComune().equals(curatore.getComune()))
                        throw new FuoriComuneException("curatore non puo' operare fuori dal suo comune");
                }
                if (materialeGenerico.getStato() != Stato.IN_ATTESA)
                    throw new UnsupportedOperationException("materiale già settato");
                Stato stato = Stato.toStatus(bStato);
                if (stato == Stato.IN_ATTESA)
                    throw new UnsupportedOperationException("non puoi impostare stato in attesa");
                materialeGenerico.setStato(stato);
                notificaService.creaNotificaApprovazione(curatore, materialeGenerico, stato);
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
    public void eliminaPuntoInteresse(String usernameCuratore, int idPuntoInteresse) throws IllegalArgumentException, FuoriComuneException {
        Optional<Curatore> oCuratore = getByUsername(usernameCuratore);
        if (oCuratore.isPresent()) {
            Curatore curatore = oCuratore.get();
            Optional<PuntoInteresse> oPoi = poiServiceImpl.getById(idPuntoInteresse);
            if (oPoi.isPresent()) {
                PuntoInteresse puntoInteresse = oPoi.get();
                if (controllaSeInComune(curatore, puntoInteresse.getComune())) {
                    poiServiceImpl.deleteById(idPuntoInteresse);
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
    public void eliminaItinerario(String usernameCuratore, int idItinerario) throws IllegalArgumentException, FuoriComuneException {
        Optional<Curatore> oCuratore = getByUsername(usernameCuratore);
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
    public void eliminaContest(String usernameCuratore, int idContest) throws IllegalArgumentException, FuoriComuneException {
        Optional<Curatore> oCuratore = getByUsername(usernameCuratore);
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
    public void eliminaMateriale(String usernameCuratore, int idMateriale) throws IllegalArgumentException, FuoriComuneException {
        Optional<Curatore> oCuratore = getByUsername(usernameCuratore);
        if (oCuratore.isPresent()) {
            Curatore curatore = oCuratore.get();
            Optional<MaterialeGenerico> oMateriale = materialeServiceImpl.getById(idMateriale);
            if (oMateriale.isPresent()) {
                MaterialeGenerico materialeGenerico = oMateriale.get();

                Optional<PuntoInteresse> oPoi = poiServiceImpl.getPoiContainingMaterial(materialeGenerico);
                if (oPoi.isPresent()) {
                    PuntoInteresse puntoInteresse = oPoi.get();
                    if (!puntoInteresse.getComune().equals(curatore.getComune())) {
                        throw new FuoriComuneException(curatore.getUsername() + " non può eliminare materiali fuori dal suo comune ");
                    }
                    puntoInteresse.rimuoviMateriale(materialeGenerico);
                    poiServiceImpl.save(puntoInteresse);
                } else {
                    Optional<Contest> oContest = contestServiceImpl.getContestContainingMaterial(materialeGenerico);
                    if (oContest.isPresent()) {
                        Contest contest = oContest.get();
                        if (!contest.getComune().equals(curatore.getComune())) {
                            throw new FuoriComuneException(curatore.getUsername() + " non può eliminare materiali fuori dal suo comune ");
                        }
                        contest.rimuoviMateriale(materialeGenerico);
                        contestServiceImpl.save(contest);
                    } else {
                        logger.warn("Il Materiale non è associato a nessun Punto o Contest");
                    }
                }
                materialeServiceImpl.deleteById(materialeGenerico.getId());
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
    public List<Notifica> getNotifiche(String usernameCuratore) throws IllegalArgumentException {
        Optional<Curatore> oCuratore = getByUsername(usernameCuratore);
        if (oCuratore.isPresent()) {
            Curatore curatore = oCuratore.get();
            return notificaService.getNotifiche(curatore);
        } else {
            logger.error("username del curatore non valido");
            throw new IllegalArgumentException("username del curatore non valido");
        }
    }


}

