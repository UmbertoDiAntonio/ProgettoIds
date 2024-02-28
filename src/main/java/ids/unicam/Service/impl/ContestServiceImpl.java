package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ContestRepository;
import ids.unicam.Service.ContestService;
import ids.unicam.exception.ContestException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class ContestServiceImpl implements ContestService {
    private final ContestRepository repository;
    private final MaterialeServiceImpl materialeService;
    private final NotificaServiceImpl notificaService;

    @Autowired
    public ContestServiceImpl(ContestRepository repository, MaterialeServiceImpl materialeService, NotificaServiceImpl notificaService) {
        this.repository = repository;
        this.materialeService = materialeService;
        this.notificaService = notificaService;
    }

    public void deleteById(int id) {
        repository.deleteById(id);
    }

    private Contest save(Contest contest) {
        return repository.save(contest);
    }

    public Optional<Contest> findById(int id) {
        return repository.findById(id);
    }

    public List<Contest> findAll() {
        return repository.findAll();
    }

    public void deleteAll() {
        repository.deleteAll();
    }

    @Override
    public Contest creaContest(Contest contest) {
        return save(contest);
    }

    @Override
    public List<Contest> getContestByPartecipante(TuristaAutenticato turistaAutenticato) {
        return repository.findContestByPartecipantiContains(turistaAutenticato);
    }

    @Override
    public List<Contest> getContestByCreatore(Animatore animatore) {
        return repository.findContestByCreatore(animatore);
    }


    @Override
    public Contest aggiungiMateriale(String usernameTurista, Integer idContest, MaterialeGenerico materialeGenerico) throws ContestException, FuoriComuneException {
        TuristaAutenticato turistaAutenticato = null;
        Optional<Contest> oContest = findById(idContest);
        if (oContest.isEmpty()) {
            throw new FuoriComuneException("id contest non valido");
        }

        Contest contest = oContest.get();
        if (contest.isExpired()) {
            throw new ContestException("il Contest e' Terminato");
        }
        if (!contest.isOpen())
            for (TuristaAutenticato turistaAutenticato1 : contest.getPartecipanti()) {
                if (turistaAutenticato1.getUsername().equals(usernameTurista))
                    turistaAutenticato = turistaAutenticato1;
            }
        if (turistaAutenticato == null && !contest.isOpen()) {
            throw new ContestException("Devi essere iscritto al contest per caricare materiale su di esso");
        }

        materialeService.aggiungiMateriale(contest, materialeGenerico);
        return save(contest);
    }


    @Override
    public List<TuristaAutenticato> getPartecipanti(Contest contest) {
        return repository.findPartecipantiByContest(contest.getId());
    }

    @Override
    @Transactional
    public void aggiungiPartecipante(Contest contest, TuristaAutenticato turistaAutenticato) {
        contest.getPartecipanti().add(turistaAutenticato);
        save(contest);
    }

    @Transactional
    public void rimuoviPartecipante(Contest contest, TuristaAutenticato turistaAutenticato) throws IllegalArgumentException{
       contest.getPartecipanti().remove(turistaAutenticato);
       save(contest);
    }

    @Transactional
    @Override
    public void setVincitoreContest(Contest contest, MaterialeGenerico materialeGenerico) {
        contest.setMaterialeVincitore(materialeGenerico);
        if(contest.getPartecipanti().contains(materialeGenerico.getCreatore()))
            notificaService.creaNotifica(contest.getCreatore(),contest,contest.getMaterialeVincitore());
        save(contest);
    }

    @Transactional
    @Override
    public void terminaContest(Contest contest, Integer idMateriale) throws ContestException {
        contest.setExpireDate(LocalDate.now());
        Optional<MaterialeGenerico> oMateriale = materialeService.getById(idMateriale);
        if (oMateriale.isEmpty()) {
            throw new ContestException("id materiale non valido");
        }
        MaterialeGenerico materiale = oMateriale.get();
        if (!contest.getMateriali().contains(materiale)) {
            throw new ContestException("il materiale non risulta tra i materiali del contest");
        }
        if(contest.getPartecipanti().contains(materiale.getCreatore()))
            setVincitoreContest(contest, materiale);
        else {
            throw new ContestException("Vincitore non valido, non puoi terminare il contest senza avere un vincitore valido");
        }
        for(TuristaAutenticato turistaAutenticato:contest.getPartecipanti())
            notificaService.creaNotifica(contest.getCreatore(),contest,turistaAutenticato);


        contest.getPartecipanti().clear();
        save(contest);
    }

    @Override
    public List<MaterialeGenerico> getMaterialiContest(Contest contenutoGenerico) {
        return repository.getMateriali(contenutoGenerico.getId());

    }
}

