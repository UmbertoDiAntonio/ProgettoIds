package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.TuristaAutenticatoRepository;
import ids.unicam.Service.TuristaAutenticatoService;
import ids.unicam.models.Invito;
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
public class TuristaAutenticatoServiceImpl implements TuristaAutenticatoService {
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


    public void deleteById(String id) {
        repository.deleteById(id);
    }


    public Optional<TuristaAutenticato> findById(String id) {
        return repository.findById(id);
    }


    public List<TuristaAutenticato> findAll() {
        return repository.findAll();
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
    public void accettaInvitoContest(TuristaAutenticato turistaAutenticato, Invito invito) {
        invitoServiceImpl.accettaInvito(turistaAutenticato, invito);
        repository.save(turistaAutenticato);
    }

    @Transactional
    @Override
    public void rimuoviPreferito(TuristaAutenticato turistaAutenticato, int id) {
        turistaAutenticato.getPreferiti().removeIf(puntoInteresse -> puntoInteresse.getId() == id);
        save(turistaAutenticato);
    }

    @Transactional
    @Override
    public void aggiungiPreferito(TuristaAutenticato turista, PuntoInteresse puntoInteresse) {
        if (Boolean.TRUE.equals(puntoInteresse.getStato().asBoolean()))
            turista.getPreferiti().add(puntoInteresse);
        save(turista);
    }

    public boolean logOut() {
        //TODO
        return true;
    }

    public List<TuristaAutenticato> findTuristiConPreferiti() {
        return repository.findTuristiConPreferiti();
    }

    @Override
    public List<PuntoInteresse> findPreferiti(TuristaAutenticato turistaAutenticato) {
        return repository.findPreferitiByTurista(turistaAutenticato.getUsername());
    }

    /**
     * Entra nel contest se è aperto
     *
     * @param contest il contest in cui si vuole entrare
     */
    @Transactional
    @Override
    public void partecipaAlContest(Contest contest, TuristaAutenticato turistaAutenticato) {
        if (!contest.isOpen()) {
            logger.error("Il contest non è aperto");
            return;
        }
        contestServiceImpl.aggiungiPartecipante(contest, turistaAutenticato);
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


    public List<Notifica> visualizzaNotifiche(TuristaAutenticato turistaAutenticato){
        return notificaService.getNotifiche(turistaAutenticato);
    }

}
