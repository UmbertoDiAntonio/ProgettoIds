package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.AnimatoreRepository;
import ids.unicam.Service.AnimatoreService;
import ids.unicam.exception.ContestException;
import ids.unicam.models.DTO.RichiestaCreazioneContestDTO;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.DTO.InvitoDTO;
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

    @Override
    public void deleteById(String id) {
        repository.deleteById(id);
    }

    @Override
    public Optional<Animatore> getById(String username) {
        return repository.findById(username);
    }

    @Override
    public Animatore update(RichiestaCreazioneContributorDTO contributorDTO, String username) {
        //TODO
        return null;
    }


    public Animatore save(Animatore animatore) {
        animatore = repository.save(animatore);
        return animatore;
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
        return invitoServiceImpl.save(new Invito(new InvitoDTO(contest, turistaAutenticato)));
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

    @Override
    public List<Animatore> getAll() {
        return repository.findAll();
    }
}
