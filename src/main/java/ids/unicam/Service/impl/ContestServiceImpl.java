package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ContestRepository;
import ids.unicam.Service.ContestService;
import ids.unicam.exception.ContestException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import jakarta.transaction.Transactional;
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

    @Autowired
    public ContestServiceImpl(ContestRepository repository, MaterialeServiceImpl materialeService) {
        this.repository = repository;
        this.materialeService = materialeService;
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
        TuristaAutenticato turistaAutenticato =null;

        Optional<Contest> oContest = findById(idContest);
        if (oContest.isEmpty()) {
            logger.error("id contest non valido");
            throw new FuoriComuneException("id contest non valido");
        }

        Contest contest = oContest.get();

        if(!contest.isOpen())
            for(TuristaAutenticato turistaAutenticato1 : contest.getPartecipanti()){
                if(turistaAutenticato1.getUsername().equals(usernameTurista))
                    turistaAutenticato=turistaAutenticato1;
            }
        if(turistaAutenticato==null && !contest.isOpen()) {
            logger.error("Devi essere iscritto al contest per caricare materiale su di esso");
            throw new ContestException("Devi essere iscritto al contest per caricare materiale su di esso");
        }
        materialeService.aggiungiMateriale(contest,materialeGenerico);
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
    @Override
    public void setVincitoreContest(Contest contest, MaterialeGenerico materialeGenerico) {
        contest.setMaterialeVincitore(materialeGenerico);
        save(contest);
    }

    @Transactional
    @Override
    public void terminaContest(Contest contest) {
        contest.setExpireDate(LocalDate.now());
        save(contest);
    }

    @Override
    public  List<MaterialeGenerico> getMaterialiContest(Contest contenutoGenerico) {
        return repository.getMateriali(contenutoGenerico.getId());

    }
}

