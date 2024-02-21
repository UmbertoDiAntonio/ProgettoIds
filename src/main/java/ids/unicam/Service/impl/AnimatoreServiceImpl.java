package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.AnimatoreRepository;
import ids.unicam.Service.AnimatoreService;
import ids.unicam.exception.ContestException;
import ids.unicam.models.DTO.RichiestaCreazioneContestDTO;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class AnimatoreServiceImpl implements AnimatoreService {
    private final AnimatoreRepository repository;
    private final ContestServiceImpl contestService;
    private final InvitoServiceImpl invitoServiceImpl;


    @Autowired
    public AnimatoreServiceImpl(AnimatoreRepository repository, ContestServiceImpl contestService, InvitoServiceImpl invitoServiceImpl, MaterialeServiceImpl materialeServiceImpl) {
        this.repository = repository;
        this.contestService = contestService;
        this.invitoServiceImpl = invitoServiceImpl;
    }

    public void deleteById(String id) {
        repository.deleteById(id);
    }


    public Animatore save(Animatore animatore) {
        animatore = repository.save(animatore);
        return animatore;
    }
    
    public Optional<Animatore> findById(String id) {
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

    @Override
    public Contest creaContest(RichiestaCreazioneContestDTO contestDTO){
        return contestService.creaContest(new Contest(contestDTO));
    }

    @Override
    public Invito invitaContest(Animatore animatore, Contest contest, TuristaAutenticato turistaAutenticato){
        if(!contest.getCreatore().equals(animatore)) {
            logger.error("L'animatore non e' il creatore del contest.");
            throw new IllegalStateException("L'animatore non e' il creatore del contest.");
        }
        if(contestService.getPartecipanti(contest).contains(turistaAutenticato)){
            logger.error("Il turista autenticato fa gia' parte del contest");
            throw new ContestException("Il turista autenticato fa gia' parte del contest");
        }
        return invitoServiceImpl.save(new Invito(contest, turistaAutenticato));
    }

    @Override
    public boolean approvaMateriale(Animatore animatore, Contest contest, MaterialeGenerico materialeGenerico, Stato stato) {
        if(!contest.getCreatore().equals(animatore)) {
            logger.warn(animatore + "  non è autorizzato ad approvare nel contest " + contest);
            return false;
        }
        if(materialeGenerico.getStato() != Stato.IN_ATTESA)
            throw new UnsupportedOperationException("materiale già settato");
        if(stato==Stato.IN_ATTESA)
            throw new UnsupportedOperationException("stato in attesa");
        contestService.approvaMateriale(materialeGenerico,stato);
        return true;
    }
}
