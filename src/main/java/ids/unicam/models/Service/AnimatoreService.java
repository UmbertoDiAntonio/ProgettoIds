package ids.unicam.models.Service;

import ids.unicam.exception.ContestException;
import ids.unicam.models.Invito;
import ids.unicam.models.Repository.AnimatoreRepository;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.utilites.Stato;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class AnimatoreService {
    private final AnimatoreRepository repository;
    private final ContestService contestService;
    private final InvitoService invitoService;
    private final MaterialeService materialeService;


    @Autowired
    public AnimatoreService(AnimatoreRepository repository, ContestService contestService, InvitoService invitoService, MaterialeService materialeService) {
        this.repository = repository;
        this.contestService = contestService;
        this.invitoService = invitoService;
        this.materialeService = materialeService;
    }

    public void deleteById(int id) {
        repository.deleteById(id);
    }


    public Animatore save(Animatore animatore) {
        animatore = repository.save(animatore);
        return animatore;
    }
    
    public Optional<Animatore> findById(int id) {
        return repository.findById(id);
    }


    public List<Animatore> findAll() {
        return repository.findAll();
    }

    public Animatore getLast() {
        return repository.findAll().getLast();
    }

    public Animatore getFirst() {
        return repository.findAll().getFirst();
    }


    public void deleteAll() {
        repository.deleteAll();
    }


    public List<Animatore> findByNomeComune(String nomeComune) {
        return repository.findByComuneNome(nomeComune);
    }

    public Contest creaContest(Animatore animatore,String nomeContest,String obiettivo,boolean tipoContest){
        return contestService.creaContest(new Contest(nomeContest,tipoContest,obiettivo,animatore));
    }

    public Invito invitaContest(Animatore animatore, Contest contest, TuristaAutenticato turistaAutenticato){
        if(!contest.getCreatore().equals(animatore)) {
            logger.error("L'animatore non e' il creatore del contest.");
            throw new IllegalStateException("L'animatore non e' il creatore del contest.");
        }
        if(contestService.getPartecipanti(contest).contains(turistaAutenticato)){
            logger.error("Il turista autenticato fa gia' parte del contest");
            throw new ContestException("Il turista autenticato fa gia' parte del contest");
        }
        return invitoService.save(new Invito(contest, turistaAutenticato));
    }

    public boolean approvaMateriale(Animatore animatore, Contest contest, MaterialeGenerico materialeGenerico, Stato stato) {
        if(!contest.getCreatore().equals(animatore)) {
            logger.warn(animatore + "  non è autorizzato ad approvare nel contest " + contest);
            return false;
        }
        if(materialeGenerico.getStato()==stato){
            return true;
        }
        if(stato == Stato.NOT_APPROVED)
            materialeService.deleteById(materialeGenerico.getId());
        contestService.approvaMateriale(materialeGenerico,stato);
        return true;
    }
}
