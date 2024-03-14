package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.TuristaAutenticatoRepository;
import ids.unicam.Service.InvitoService;
import ids.unicam.Service.PoiService;
import ids.unicam.Service.TuristaAutenticatoService;
import ids.unicam.exception.ContestException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Invito;
import ids.unicam.models.Observer;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contenitore;
import ids.unicam.models.contenuti.Contest;
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
public class TuristaAutenticatoServiceImpl implements TuristaAutenticatoService, Observer {
    private final TuristaAutenticatoRepository repository;
    private final ContestServiceImpl contestService;
    private final InvitoService invitoService;
    private final NotificaService notificaService;
    private final PoiService poiService;
    private final MaterialeServiceImpl materialeService;

    @Autowired
    public TuristaAutenticatoServiceImpl(TuristaAutenticatoRepository repository, ContestServiceImpl contestService, InvitoService invitoService, NotificaService notificaService, PoiService poiService, MaterialeServiceImpl materialeService) {
        this.repository = repository;
        this.contestService = contestService;
        this.invitoService = invitoService;
        this.notificaService = notificaService;
        this.poiService = poiService;
        this.materialeService = materialeService;
    }


    @Override
    public void deleteByUsername(@NotNull String id) {
        repository.deleteById(id);
    }


    @Override
    public @NotNull TuristaAutenticato save(@NotNull TuristaAutenticato turistaAutenticato) {
        turistaAutenticato = repository.save(turistaAutenticato);
        return turistaAutenticato;
    }


    @Transactional
    @Override
    public void accettaInvitoContest(@NotNull String usernameUtente, int idInvito) throws IllegalArgumentException, ContestException {
        Optional<TuristaAutenticato> oTurista = getByUsername(usernameUtente);
        if (oTurista.isEmpty()) {
            logger.warn("username Utente non valido");
            throw new IllegalArgumentException("username Utente non valido");
        }
        TuristaAutenticato turistaAutenticato = oTurista.get();
        Optional<Invito> oInvito = invitoService.findById(idInvito);
        if (oInvito.isEmpty()) {
            logger.warn("id Invito non valido");
            throw new IllegalArgumentException("id Invito non valido");
        }
        Invito invito = oInvito.get();
        invitoService.accettaInvito(turistaAutenticato, invito);
    }


    @Transactional
    @Override
    public void rimuoviPreferito(@NotNull String usernameTurista, int idPunto) throws IllegalArgumentException {
        Optional<TuristaAutenticato> oTurista = getByUsername(usernameTurista);
        if (oTurista.isPresent()) {
            TuristaAutenticato turistaAutenticato = oTurista.get();
            turistaAutenticato.removeIfPreferito(puntoInteresse -> puntoInteresse.getId() == idPunto);
            save(turistaAutenticato);
        } else {
            logger.warn("username del turista non valido");
            throw new IllegalArgumentException("username del turista non valido");
        }
    }


    @Transactional
    @Override
    public void aggiungiMateriale(@NotNull String usernameTurista, int idContenitore, @NotNull MaterialeGenerico materialeGenerico) throws IllegalArgumentException, IllegalStateException, ContestException, FuoriComuneException {
        if (poiService.getById(idContenitore).isPresent()) {
            Optional<TuristaAutenticato> oTurista = getByUsername(usernameTurista);
            if (oTurista.isEmpty()) {
                logger.warn("username non valido");
                throw new IllegalArgumentException("username non valido");
            }
            TuristaAutenticato turistaAutenticato = oTurista.get();

            Optional<PuntoInteresse> oPunto = poiService.getById(idContenitore);
            if (oPunto.isEmpty()) {
                logger.warn("id punto interesse non valido");
                throw new IllegalArgumentException("id punto interesse non valido");
            }
            PuntoInteresse puntoInteresse = oPunto.get();

            if (turistaAutenticato instanceof Contributor contributor) {
                if (!contributor.getComune().equals(puntoInteresse.getComune())) {
                    throw new FuoriComuneException("il contributor cerca di caricare il materiale fuori dal suo comune");
                }
            }
            if (Boolean.FALSE.equals(puntoInteresse.getStato().asBoolean())) {
                logger.warn("il contributor cerca di caricare il materiale su un punto di interesse non approvato");
                throw new IllegalStateException("il contributor cerca di caricare il materiale su un punto di interesse non approvato");
            }
            aggiungiMateriale(puntoInteresse, materialeGenerico);
            poiService.save(puntoInteresse);
        } else {
            TuristaAutenticato turistaAutenticato = null;
            Optional<Contest> oContest = contestService.findById(idContenitore);
            if (oContest.isEmpty()) {
                throw new IllegalArgumentException("id contest non valido");
            }

            Contest contest = oContest.get();
            if (contest.isExpired()) {
                throw new ContestException("il Contest e' Terminato");
            }

            if (contest.isOpen()) {
                contestService.aggiungiPartecipante(contest, materialeGenerico.getCreatore());
            }
            if (!contest.isOpen())
                for (TuristaAutenticato turistaAutenticato1 : contest.getPartecipanti()) {
                    if (turistaAutenticato1.getUsername().equals(usernameTurista)) {
                        turistaAutenticato = turistaAutenticato1;
                    }
                }
            if (turistaAutenticato == null && !contest.isOpen()) {
                throw new ContestException("Devi essere iscritto al contest per caricare materiale su di esso");
            }

            aggiungiMateriale(contest, materialeGenerico);
            contestService.save(contest);
        }
    }

    private void aggiungiMateriale(@NotNull Contenitore contenitore, @NotNull MaterialeGenerico materialeGenerico) {
        contenitore.aggiungiMateriale(materialeGenerico);
        materialeService.save(materialeGenerico);
    }


    @Transactional
    @Override
    public void aggiungiPreferito(@NotNull String usernameTurista, int idPunto) throws IllegalArgumentException {
        Optional<PuntoInteresse> oPunto = poiService.getById(idPunto);
        if (oPunto.isEmpty()) {
            logger.warn("id punto non valido");
            throw new IllegalArgumentException("id punto non valido");
        }
        PuntoInteresse puntoInteresse = oPunto.get();
        if (Boolean.TRUE.equals(puntoInteresse.getStato().asBoolean())) {
            Optional<TuristaAutenticato> oTurista = getByUsername(usernameTurista);
            if (oTurista.isPresent()) {
                TuristaAutenticato turistaAutenticato = oTurista.get();
                turistaAutenticato.addPreferito(puntoInteresse);
                save(turistaAutenticato);
            } else {
                logger.warn("username del turista non valido");
                throw new IllegalArgumentException("username del turista non valido");
            }
        } else {
            logger.warn("punto di interesse non approvato");
            throw new IllegalArgumentException("punto di interesse non approvato");
        }

    }


    @Override
    public @NotNull List<PuntoInteresse> findPreferiti(@NotNull String usernameTurista) throws IllegalArgumentException {
        Optional<TuristaAutenticato> oTurista = getByUsername(usernameTurista);
        if (oTurista.isPresent()) {
            TuristaAutenticato turistaAutenticato = oTurista.get();
            return repository.findPreferitiByTurista(turistaAutenticato.getUsername());
        }
        logger.warn("username del turista non valido");
        throw new IllegalArgumentException("username del turista non valido");
    }


    @Transactional
    @Override
    public void partecipaAlContest(int idContest, @NotNull String usernameTurista) throws IllegalArgumentException, ContestException {
        Optional<Contest> oContest = contestService.findById(idContest);
        if (oContest.isPresent()) {
            Contest contest = oContest.get();
            Optional<TuristaAutenticato> oTurista = getByUsername(usernameTurista);
            if (oTurista.isPresent()) {
                TuristaAutenticato turistaAutenticato = oTurista.get();
                if (!contest.isOpen()) {
                    throw new ContestException("Il contest non è aperto");
                }
                contestService.aggiungiPartecipante(contest, turistaAutenticato);
            } else {
                logger.warn("username del turista non valido");
                throw new IllegalArgumentException("username del turista non valido");
            }
        } else {
            logger.warn("id del contest non valido");
            throw new IllegalArgumentException("id del contest non valido");
        }
    }


    @Transactional
    @Override
    public void cancellaPartecipazioneContest(int idContest, @NotNull String usernameTurista) throws IllegalArgumentException {
        Optional<Contest> oContest = contestService.findById(idContest);
        if (oContest.isPresent()) {
            Contest contest = oContest.get();
            Optional<TuristaAutenticato> oTurista = getByUsername(usernameTurista);
            if (oTurista.isPresent()) {
                TuristaAutenticato turistaAutenticato = oTurista.get();
                if (!contest.getPartecipanti().contains(turistaAutenticato)) {
                    logger.warn("Non sei un membro del contest");
                } else {
                    contest.rimuoviPartecipante(turistaAutenticato);
                    contestService.save(contest);
                }
            } else {
                logger.warn("username del turista non valido");
                throw new IllegalArgumentException("username del turista non valido");
            }
        } else {
            logger.warn("id del contest non valido");
            throw new IllegalArgumentException("id del contest non valido");
        }
    }

    @Override
    public boolean isUsernameUnique(@NotNull String username) {
        return repository.countUsername(username) == 0;
    }

    @Override
    public @NotNull List<TuristaAutenticato> getAll() {
        return repository.findAll();
    }

    @Override
    public @NotNull Optional<TuristaAutenticato> getByUsername(@NotNull String username) {
        return repository.findById(username);
    }


    @Override
    public @NotNull List<Notifica> visualizzaNotifiche(@NotNull String usernameTurista) throws IllegalArgumentException {
        Optional<TuristaAutenticato> oTurista = getByUsername(usernameTurista);
        if (oTurista.isPresent()) {
            TuristaAutenticato turistaAutenticato = oTurista.get();
            return notificaService.getNotifiche(turistaAutenticato);
        }
        logger.warn("username del turista non valido");
        throw new IllegalArgumentException("username del turista non valido");
    }


    @Override
    public @NotNull List<Invito> getInviti(@NotNull String usernameTurista) {
        return invitoService.getInvitiRicevuti(usernameTurista);
    }


    @Override
    @Transactional
    public void deleteNotificheByUsername(@NotNull String usernameTurista) {
        notificaService.rimuoviNotificheByUsername(usernameTurista);
    }
}
