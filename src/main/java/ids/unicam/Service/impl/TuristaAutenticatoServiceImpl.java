package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.TuristaAutenticatoRepository;
import ids.unicam.Service.TuristaAutenticatoService;
import ids.unicam.Service.TuristaService;
import ids.unicam.models.DTO.InvitoDTO;
import ids.unicam.models.DTO.PuntoInteresseDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class TuristaAutenticatoServiceImpl implements TuristaAutenticatoService {
    private final TuristaAutenticatoRepository repository;
    private final ContestServiceImpl contestServiceImpl;
    private final InvitoServiceImpl invitoServiceImpl;
    private final NotificaServiceImpl notificaService;
    private final PoiServiceImpl poiService;


    @Autowired
    public TuristaAutenticatoServiceImpl(TuristaAutenticatoRepository repository, ContestServiceImpl contestServiceImpl, InvitoServiceImpl invitoServiceImpl, NotificaServiceImpl notificaService, PoiServiceImpl poiService) {
        this.repository = repository;
        this.contestServiceImpl = contestServiceImpl;
        this.invitoServiceImpl = invitoServiceImpl;
        this.notificaService = notificaService;
        this.poiService = poiService;
    }

    @Override
    public void deleteById(String id) {
        repository.deleteById(id);
    }

    @Override
    public TuristaAutenticato update(TuristaAutenticatoDTO turistaDTO, String username) {
        //TODO
        return null;
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
    public void accettaInvitoContest(TuristaAutenticatoDTO turistaDTO, InvitoDTO invitoDTO) {
        invitoServiceImpl.accettaInvito(turistaDTO, invitoDTO);
        repository.save(new TuristaAutenticato(turistaDTO));
    }

    @Transactional
    @Override
    public void rimuoviPreferito(String usernameTurista, int id) {
        Optional<TuristaAutenticato> oTurista = findTuristaByUsername(usernameTurista);
        if (oTurista.isPresent()) {
            TuristaAutenticato turistaAutenticato = oTurista.get();
            turistaAutenticato.getPreferiti().removeIf(puntoInteresse -> puntoInteresse.getId() == id);
            save(turistaAutenticato);
        }
        logger.error("username del turista non valido");
        throw new IllegalArgumentException("username del turista non valido");
    }

    @Transactional
    @Override
    public void aggiungiPreferito(String usernameTurista, int idPunto) {
        Optional<PuntoInteresse> oPunto = poiService.findById(idPunto);
        if (oPunto.isPresent()) {
            PuntoInteresse puntoInteresse = oPunto.get();
            if (Boolean.TRUE.equals(puntoInteresse.getStato().asBoolean())) {
                Optional<TuristaAutenticato> oTurista = findTuristaByUsername(usernameTurista);
                if (oTurista.isPresent()) {
                    TuristaAutenticato turistaAutenticato = oTurista.get();
                    turistaAutenticato.getPreferiti().add(puntoInteresse);
                    save(turistaAutenticato);
                }
            }
            logger.error("username del turista non valido");
            throw new IllegalArgumentException("username del turista non valido");
        }
        logger.error("id del punto di interesse non valido");
        throw new IllegalArgumentException("id del punto di interesse non valido");
    }

    public boolean logOut() {
        //TODO
        return true;
    }

    public List<TuristaAutenticato> findTuristiConPreferiti() {
        return repository.findTuristiConPreferiti();
    }

    @Override
    public List<PuntoInteresse> findPreferiti(String usernameTurista) {
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
    public void partecipaAlContest(Integer idContest, String usernameTurista) {
        Optional<Contest> oContest = contestServiceImpl.findById(idContest);
        if (oContest.isPresent()) {
            Contest contest=oContest.get();
            Optional<TuristaAutenticato> oTurista = findTuristaByUsername(usernameTurista);
            if (oTurista.isPresent()) {
                TuristaAutenticato turistaAutenticato = oTurista.get();
                if (!contest.isOpen()) {
                    logger.error("Il contest non è aperto");
                    return;
                }
                contestServiceImpl.aggiungiPartecipante(contest, turistaAutenticato);
            }
            logger.error("username del turista non valido");
            throw new IllegalArgumentException("username del turista non valido");
        }
        logger.error("id del contest non valido");
        throw new IllegalArgumentException("id del contest non valido");
    }

    @Override
    public Optional<TuristaAutenticato> findTuristaByUsername(String username) {
        return repository.findByUsername(username);
    }

    @Override
    public boolean verificaPassword(String password, String username) {
        Optional<TuristaAutenticato> turista = findTuristaByUsername(username);
        return turista.map(turistaAutenticato -> turistaAutenticato.getPassword().equals(password)).orElse(true);
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


    public List<Notifica> visualizzaNotifiche(String usernameTurista) {
        Optional<TuristaAutenticato> oTurista = findTuristaByUsername(usernameTurista);
        if (oTurista.isPresent()) {
            TuristaAutenticato turistaAutenticato = oTurista.get();
            return notificaService.getNotifiche(turistaAutenticato);
        }
        logger.error("username del turista non valido");
        throw new IllegalArgumentException("username del turista non valido");
    }

}
