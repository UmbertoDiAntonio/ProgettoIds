package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.TuristaAutenticatoRepository;
import ids.unicam.Service.TuristaAutenticatoService;
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
    private final ContestServiceImpl contestServiceImpl;
    private final InvitoServiceImpl invitoServiceImpl;
    private final NotificaServiceImpl notificaService;


    @Autowired
    public TuristaAutenticatoServiceImpl(TuristaAutenticatoRepository repository, ContestServiceImpl contestServiceImpl, InvitoServiceImpl invitoServiceImpl, NotificaServiceImpl notificaService) {
        this.repository = repository;
        this.contestServiceImpl = contestServiceImpl;
        this.invitoServiceImpl = invitoServiceImpl;
        this.notificaService = notificaService;
    }

    @Override
    public void deleteById(String id) {
        repository.deleteById(id);
    }

    public void deleteAll() {
        repository.deleteAll();
    }

    public TuristaAutenticato save(TuristaAutenticato turistaAutenticato) {
        turistaAutenticato = repository.save(turistaAutenticato);
        return turistaAutenticato;
    }

    @Transactional
    @Override
    public void accettaInvitoContest(TuristaAutenticato turistaAutenticato, Invito invito) throws IllegalArgumentException {
        invitoServiceImpl.accettaInvito(turistaAutenticato, invito);
    }

    @Transactional
    @Override
    public void rimuoviPreferito(String usernameTurista, int id) throws IllegalArgumentException {
        Optional<TuristaAutenticato> oTurista = findTuristaByUsername(usernameTurista);
        if (oTurista.isPresent()) {
            TuristaAutenticato turistaAutenticato = oTurista.get();
            turistaAutenticato.getPreferiti().removeIf(puntoInteresse -> puntoInteresse.getId() == id);
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
            Optional<TuristaAutenticato> oTurista = findTuristaByUsername(usernameTurista);
            if (oTurista.isPresent()) {
                TuristaAutenticato turistaAutenticato = oTurista.get();
                turistaAutenticato.getPreferiti().add(puntoInteresse);
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
        Optional<TuristaAutenticato> oTurista = findTuristaByUsername(usernameTurista);
        if (oTurista.isPresent()) {
            TuristaAutenticato turistaAutenticato = oTurista.get();
            return repository.findPreferitiByTurista(turistaAutenticato.getUsername());
        }
        logger.error("username del turista non valido");
        throw new IllegalArgumentException("username del turista non valido");
    }


    @Transactional
    @Override
    public void partecipaAlContest(Integer idContest, String usernameTurista) throws UnsupportedOperationException, IllegalArgumentException {
        Optional<Contest> oContest = contestServiceImpl.findById(idContest);
        if (oContest.isPresent()) {
            Contest contest = oContest.get();
            Optional<TuristaAutenticato> oTurista = findTuristaByUsername(usernameTurista);
            if (oTurista.isPresent()) {
                TuristaAutenticato turistaAutenticato = oTurista.get();
                if (!contest.isOpen()) {
                    logger.error("Il contest non è aperto");
                    throw new UnsupportedOperationException("Il contest non è aperto");
                }
                contestServiceImpl.aggiungiPartecipante(contest, turistaAutenticato);
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
    public void cancellaPartecipazioneContest(Integer idContest, String usernameTurista) throws IllegalArgumentException {
        Optional<Contest> oContest = contestServiceImpl.findById(idContest);
        if (oContest.isPresent()) {
            Contest contest = oContest.get();
            Optional<TuristaAutenticato> oTurista = findTuristaByUsername(usernameTurista);
            if (oTurista.isPresent()) {
                TuristaAutenticato turistaAutenticato = oTurista.get();
                if (!contest.getPartecipanti().contains(turistaAutenticato)) {
                    logger.warn("Non sei un membro del contest");
                } else {
                    contestServiceImpl.rimuoviPartecipante(contest, turistaAutenticato);
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
    public Optional<TuristaAutenticato> findTuristaByUsername(String username) {
        return repository.findByUsername(username);
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
    public Optional<TuristaAutenticato> getById(String username) {
        return repository.findById(username);
    }


    @Override
    public List<Notifica> visualizzaNotifiche(String usernameTurista) throws IllegalArgumentException {
        Optional<TuristaAutenticato> oTurista = findTuristaByUsername(usernameTurista);
        if (oTurista.isPresent()) {
            TuristaAutenticato turistaAutenticato = oTurista.get();
            return notificaService.getNotifiche(turistaAutenticato);
        }
        logger.error("username del turista non valido");
        throw new IllegalArgumentException("username del turista non valido");
    }

    @Override
    public List<Invito> getInviti(String usernameTurista) {
        return invitoServiceImpl.getInvitiRicevuti(usernameTurista);
    }

    @Override
    @Transactional
    public void deleteNotificheById(String usernameTurista) {
        notificaService.rimuoviNotificheByUsername(usernameTurista);
    }


}
