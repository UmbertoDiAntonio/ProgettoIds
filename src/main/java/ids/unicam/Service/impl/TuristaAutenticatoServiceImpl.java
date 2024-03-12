package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.TuristaAutenticatoRepository;
import ids.unicam.Service.ContestService;
import ids.unicam.Service.InvitoService;
import ids.unicam.Service.TuristaAutenticatoService;
import ids.unicam.exception.ContestException;
import ids.unicam.models.Invito;
import ids.unicam.models.Observer;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class TuristaAutenticatoServiceImpl implements TuristaAutenticatoService, Observer {
    private final TuristaAutenticatoRepository repository;
    private final ContestService contestService;
    private final InvitoService invitoService;
    private final NotificaService notificaService;

    @Autowired
    public TuristaAutenticatoServiceImpl(TuristaAutenticatoRepository repository, ContestService contestService, InvitoService invitoService, NotificaService notificaService) {
        this.repository = repository;
        this.contestService = contestService;
        this.invitoService = invitoService;
        this.notificaService = notificaService;
    }


    @Override
    public void deleteByUsername(String id) {
        repository.deleteById(id);
    }


    @Override
    public TuristaAutenticato save(TuristaAutenticato turistaAutenticato) {
        turistaAutenticato = repository.save(turistaAutenticato);
        return turistaAutenticato;
    }

    @Transactional
    @Override
    public void accettaInvitoContest(String usernameUtente, int idInvito) throws IllegalArgumentException, ContestException {
        Optional<TuristaAutenticato> oTurista = getByUsername(usernameUtente);
        if (oTurista.isEmpty()) {
            logger.error("username Utente non valido");
            throw  new IllegalArgumentException("username Utente non valido");
        }
        TuristaAutenticato turistaAutenticato = oTurista.get();
        Optional<Invito> oInvito = invitoService.findById(idInvito);
        if (oInvito.isEmpty()) {
            logger.error("id Invito non valido");
            throw  new IllegalArgumentException("id Invito non valido");
        }
        Invito invito = oInvito.get();
        invitoService.accettaInvito(turistaAutenticato, invito);
    }

    @Transactional
    @Override
    public void rimuoviPreferito(String usernameTurista, int id) throws IllegalArgumentException {
        Optional<TuristaAutenticato> oTurista = getByUsername(usernameTurista);
        if (oTurista.isPresent()) {
            TuristaAutenticato turistaAutenticato = oTurista.get();
            turistaAutenticato.removeIfPreferito(puntoInteresse -> puntoInteresse.getId() == id);
            save(turistaAutenticato);
        } else {
            logger.error("username del turista non valido");
            throw new IllegalArgumentException("username del turista non valido");
        }
    }

    @Transactional
    @Override
    public void aggiungiPreferito(String usernameTurista, PuntoInteresse puntoInteresse) throws IllegalArgumentException {
        if (Boolean.TRUE.equals(puntoInteresse.getStato().asBoolean())) {
            Optional<TuristaAutenticato> oTurista = getByUsername(usernameTurista);
            if (oTurista.isPresent()) {
                TuristaAutenticato turistaAutenticato = oTurista.get();
                turistaAutenticato.addPreferito(puntoInteresse);
                save(turistaAutenticato);
            } else {
                logger.error("username del turista non valido");
                throw new IllegalArgumentException("username del turista non valido");
            }
        } else {
            logger.error("punto di interesse non approvato");
            throw new IllegalArgumentException("punto di interesse non approvato");
        }

    }

    @Override
    public List<PuntoInteresse> findPreferiti(String usernameTurista) throws IllegalArgumentException {
        Optional<TuristaAutenticato> oTurista = getByUsername(usernameTurista);
        if (oTurista.isPresent()) {
            TuristaAutenticato turistaAutenticato = oTurista.get();
            return repository.findPreferitiByTurista(turistaAutenticato.getUsername());
        }
        logger.error("username del turista non valido");
        throw new IllegalArgumentException("username del turista non valido");
    }


    @Transactional
    @Override
    public void partecipaAlContest(int idContest, String usernameTurista) throws IllegalArgumentException, ContestException {
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
                logger.error("username del turista non valido");
                throw new IllegalArgumentException("username del turista non valido");
            }
        } else {
            logger.error("id del contest non valido");
            throw new IllegalArgumentException("id del contest non valido");
        }
    }

    @Transactional
    @Override
    public void cancellaPartecipazioneContest(int idContest, String usernameTurista) throws IllegalArgumentException {
        Optional<Contest> oContest = contestService.findById(idContest);
        if (oContest.isPresent()) {
            Contest contest = oContest.get();
            Optional<TuristaAutenticato> oTurista = getByUsername(usernameTurista);
            if (oTurista.isPresent()) {
                TuristaAutenticato turistaAutenticato = oTurista.get();
                if (!contest.getPartecipanti().contains(turistaAutenticato)) {
                    logger.warn("Non sei un membro del contest");
                } else {
                    contestService.rimuoviPartecipante(contest, turistaAutenticato);
                }
            } else {
                logger.error("username del turista non valido");
                throw new IllegalArgumentException("username del turista non valido");
            }
        } else {
            logger.error("id del contest non valido");
            throw new IllegalArgumentException("id del contest non valido");
        }
    }

    @Override
    public boolean isUsernameUnique(String username) {
        return repository.countUsername(username) == 0;
    }

    @Override
    public List<TuristaAutenticato> getAll() {
        return repository.findAll();
    }

    @Override
    public Optional<TuristaAutenticato> getByUsername(String username) {
        return repository.findById(username);
    }


    @Override
    public List<Notifica> visualizzaNotifiche(String usernameTurista) throws IllegalArgumentException {
        Optional<TuristaAutenticato> oTurista = getByUsername(usernameTurista);
        if (oTurista.isPresent()) {
            TuristaAutenticato turistaAutenticato = oTurista.get();
            return notificaService.getNotifiche(turistaAutenticato);
        }
        logger.error("username del turista non valido");
        throw new IllegalArgumentException("username del turista non valido");
    }

    @Override
    public List<Invito> getInviti(String usernameTurista) {
        return invitoService.getInvitiRicevuti(usernameTurista);
    }

    @Override
    @Transactional
    public void deleteNotificheByUsername(String usernameTurista) {
        notificaService.rimuoviNotificheByUsername(usernameTurista);
    }
}
