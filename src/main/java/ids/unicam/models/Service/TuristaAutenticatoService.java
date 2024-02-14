package ids.unicam.models.Service;

import ids.unicam.models.Invito;
import ids.unicam.models.Repository.TuristaAutenticatoRepository;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class TuristaAutenticatoService {
    private final TuristaAutenticatoRepository repository;
    private final MaterialeService materialeService;
    private final ContestService contestService;
    private final InvitoService invitoService;

    @Autowired
    public TuristaAutenticatoService(TuristaAutenticatoRepository repository, MaterialeService materialeService, ContestService contestService, InvitoService invitoService) {
        this.repository = repository;
        this.materialeService = materialeService;
        this.contestService = contestService;
        this.invitoService = invitoService;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }


    public TuristaAutenticato save(TuristaAutenticato turistaAutenticato) {
        turistaAutenticato = repository.save(turistaAutenticato);
        return turistaAutenticato;
    }


    public Optional<TuristaAutenticato> findById(int id) {
        return repository.findById(id);
    }


    public List<TuristaAutenticato> findAll() {
        return repository.findAll();
    }

    public TuristaAutenticato getLast() {
        return repository.findAll().getLast();
    }

    public TuristaAutenticato getFirst() {
        return repository.findAll().getFirst();
    }


    public void deleteAll() {
        repository.deleteAll();
    }

    public void accettaInvitoContest(TuristaAutenticato turistaAutenticato, Invito invito) {
        invitoService.accettaInvito(turistaAutenticato, invito);
        repository.save(turistaAutenticato);
    }

    @Transactional
    public void rimuoviPreferito(TuristaAutenticato turistaAutenticato, int id) {
        turistaAutenticato.getPreferiti().removeIf(puntoInteresse -> puntoInteresse.getId()==id);
        save(turistaAutenticato);
    }

    public void aggiungiPreferito(TuristaAutenticato turista, PuntoInteresse puntoInteresse) {
        if (puntoInteresse.getStato().asBoolean())
            turista.getPreferiti().add(puntoInteresse);
        save(turista);
    }

    public List<TuristaAutenticato> findTuristiConPreferiti() {
        return repository.findTuristiConPreferiti();
    }

    public List<PuntoInteresse> findPreferiti(TuristaAutenticato turistaAutenticato){
        return repository.findPreferitiByTurista(turistaAutenticato.getId());
    }
}
