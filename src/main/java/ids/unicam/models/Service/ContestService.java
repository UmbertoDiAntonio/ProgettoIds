package ids.unicam.models.Service;

import ids.unicam.exception.ContestException;
import ids.unicam.models.Invito;
import ids.unicam.models.Repository.ContestRepository;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Stato;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class ContestService {
    private final ContestRepository repository;
    private final MaterialeService materialeService;

    @Autowired
    public ContestService(ContestRepository repository, MaterialeService materialeService) {
        this.repository = repository;
        this.materialeService = materialeService;

    }

    public List<TuristaAutenticato> getPartecipanti(Contest contest) {
        return repository.findPartecipantiByContest(contest.getId());
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

    public Contest creaContest(Contest contest) {
        return save(contest);
    }

    public List<Contest> getContestByPartecipante(TuristaAutenticato turistaAutenticato) {
        return repository.findContestByPartecipantiContains(turistaAutenticato);
    }

    public List<Contest> getContestByCreatore(Animatore animatore) {
        return repository.findContestByCreatore(animatore);
    }

    public void approvaMateriale(MaterialeGenerico materialeGenerico, Stato stato) {
        materialeService.approvaMateriale(materialeGenerico, stato);
    }

    public List<MaterialeGenerico> getMaterialiContest(Contest contest) {
        return materialeService.findByWhere(contest);
    }

    public MaterialeGenerico aggiungiMateriale(MaterialeGenerico materialeGenerico, Contest contest, TuristaAutenticato turistaAutenticato){
        if(!getPartecipanti(contest).contains(turistaAutenticato)) {
            logger.error("Devi essere iscritto al contest per caricare materiale su di esso");
            throw new ContestException("Devi essere iscritto al contest per caricare materiale su di esso");
        }
        materialeGenerico.setIdProprietario(contest.getId());
        return materialeService.save(materialeGenerico);
    }


    @Transactional
    public void aggiungiPartecipante(Contest contest, TuristaAutenticato turistaAutenticato) {
        Optional<Contest> contest1 = findById(contest.getId());
        if(contest1.isPresent()){
            contest1.get().getPartecipanti().add(turistaAutenticato);
            save(contest1.get());
        }
    }
}
