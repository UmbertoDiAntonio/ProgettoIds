package ids.unicam.models.Service;

import ids.unicam.models.Invito;
import ids.unicam.models.Repository.ContestRepository;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Stato;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class ContestService {
    private final ContestRepository repository;
    private final MaterialeService materialeService;
    private final InvitoService invitoService;

    @Autowired
    public ContestService(ContestRepository repository, MaterialeService materialeService, InvitoService invitoService) {
        this.repository = repository;
        this.materialeService = materialeService;
        this.invitoService = invitoService;
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

    /**
     * Verifica se l'invito è valido, controlla se il contest è solo su invito e se il turista non è già entrato
     *
     * @return true se l'invito è valido
     */
    public boolean isValid(Invito invito) {
        return invitoService.isValid(invito);
    }

}
