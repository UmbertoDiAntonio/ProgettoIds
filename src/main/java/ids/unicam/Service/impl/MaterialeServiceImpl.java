package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.MaterialeRepository;
import ids.unicam.Service.MaterialeService;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class MaterialeServiceImpl implements MaterialeService {

    private final MaterialeRepository repository;


    @Autowired
    public MaterialeServiceImpl(MaterialeRepository repository) {
        this.repository = repository;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }



    public MaterialeGenerico save(MaterialeGenerico materialeGenerico) {
        return repository.save(materialeGenerico);
    }


    public Optional<MaterialeGenerico> findById(int id) {
        return repository.findById(id);
    }

    public List<MaterialeGenerico> findAll() {
        return repository.findAll();
    }

    public MaterialeGenerico getLast() {
        return repository.findAll().getLast();
    }

    public MaterialeGenerico getFirst() {
        return repository.findAll().getFirst();
    }


    public void deleteAll() {
        repository.deleteAll();
    }

    @Override
    public void approvaMateriale(MaterialeGenerico materialeGenerico, Stato stato) {
        materialeGenerico.setStato(stato);
        save(materialeGenerico);
    }




}
