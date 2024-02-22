package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.MaterialeRepository;
import ids.unicam.Service.MaterialeService;
import ids.unicam.models.DTO.MaterialeDTO;
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

    @Override
    public void deleteById(int id) {
        repository.deleteById(id);
    }

    @Override
    public Optional<MaterialeGenerico> getById(int id) {
        return repository.findById(id);
    }

    @Override
    public MaterialeGenerico update(MaterialeDTO materialeDTO, int id) {
        //TODO
        return null;
    }

    @Override
    public List<MaterialeGenerico> getAll() {
        return repository.findAll();
    }



    public MaterialeGenerico save(MaterialeGenerico materialeGenerico) {
        return repository.save(materialeGenerico);
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
