package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ContestRepository;
import ids.unicam.Service.ContestService;
import ids.unicam.exception.ContestException;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class ContestServiceImpl implements ContestService {
    private final ContestRepository repository;
    private final MaterialeServiceImpl materialeServiceImpl;

    @Autowired
    public ContestServiceImpl(ContestRepository repository, MaterialeServiceImpl materialeServiceImpl) {
        this.repository = repository;
        this.materialeServiceImpl = materialeServiceImpl;

    }

    public void deleteById(int id) {
        repository.deleteById(id);
    }

    Contest save(Contest contest) {
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
    public void approvaMateriale(MaterialeGenerico materialeGenerico, Stato stato) {
        materialeServiceImpl.approvaMateriale(materialeGenerico, stato);
    }


    @Override
    public void aggiungiMateriale(MaterialeGenerico materialeGenerico, Contest contest, TuristaAutenticato turistaAutenticato) {
        if (!(getPartecipanti(contest).contains(turistaAutenticato))) {
            logger.error("Devi essere iscritto al contest per caricare materiale su di esso");
            throw new ContestException("Devi essere iscritto al contest per caricare materiale su di esso");
        }
        contest.addMateriale(materialeGenerico);
        materialeServiceImpl.save(materialeGenerico);
        save(contest);
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

    @Override
    public  List<MaterialeGenerico> getMaterialiContest(Contest contenutoGenerico) {
        return repository.getMateriali(contenutoGenerico.getId());

    }
}

