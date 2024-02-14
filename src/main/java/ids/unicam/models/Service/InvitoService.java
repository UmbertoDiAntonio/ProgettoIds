package ids.unicam.models.Service;

import ids.unicam.models.Invito;
import ids.unicam.models.Repository.InvitoRepository;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class InvitoService {
    private final InvitoRepository repository;
    private final ContestService contestService;

    @Autowired
    public InvitoService(InvitoRepository repository, ContestService contestService) {
        this.repository = repository;
        this.contestService = contestService;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }


    public Invito save(Invito invito) {
        return repository.save(invito);
    }


    public Optional<Invito> findById(int id) {
        return repository.findById(id);
    }


    public List<Invito> findAll() {
        return repository.findAll();
    }

    public Invito getLast() {
        return repository.findAll().getLast();
    }

    public Invito getFirst() {
        return repository.findAll().getFirst();
    }


    public void deleteAll() {
        repository.deleteAll();
    }

    public void accettaInvito(TuristaAutenticato turistaAutenticato,Invito invito){
        for (Invito inv : findByInvitato(turistaAutenticato)) {
            if (inv.getId() == invito.getId()) {
                Contest contest = invito.getContest();
                contestService.getPartecipanti(contest).add(turistaAutenticato);
            }
        }
    }

    private List<Invito> findByInvitato(TuristaAutenticato invitato) {
        return repository.findByInvitato(invitato);
    }

    public boolean isValid(Invito invito) {
        return !invito.getContest().isOpen() || !contestService.getPartecipanti(invito.getContest()).contains(invito.getInvitato());
    }

    public List<Invito> getInvitiRicevuti(TuristaAutenticato turistaAutenticato) {
        return repository.findInvitiByTurista(turistaAutenticato.getId());
    }
}
