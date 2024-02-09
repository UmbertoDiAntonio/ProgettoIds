package ids.unicam.models.Service;

import ids.unicam.models.Repository.AnimatorereRepository;
import ids.unicam.models.attori.Animatore;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class AnimatoreService {
    private final AnimatorereRepository repository;

    @Autowired
    public AnimatoreService(AnimatorereRepository repository) {
        this.repository = repository;
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
}
