package ids.unicam.models.Service;

import ids.unicam.models.Repository.MaterialeRepository;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.ContenutoGenerico;
import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.utilites.Stato;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class MaterialeService {
    private final MaterialeRepository repository;


    @Autowired
    public MaterialeService(MaterialeRepository repository) {
        this.repository = repository;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }


    public MaterialeGenerico save(TuristaAutenticato caricatore, MaterialeGenerico materialeGenerico) {
        if(caricatore instanceof ContributorAutorizzato) materialeGenerico.setStato(Stato.APPROVED);
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

    public void approvaMateriale(MaterialeGenerico materialeGenerico,Stato stato) {
        materialeGenerico.setStato(stato);
    }


    public List<MaterialeGenerico> findByWhere(ContenutoGenerico contenutoGenerico) {
        return repository.findByIdProprietario(contenutoGenerico.getId());
    }

    public MaterialeGenerico save(MaterialeGenerico materialeGenerico){
        return repository.save(materialeGenerico);
    }

}
