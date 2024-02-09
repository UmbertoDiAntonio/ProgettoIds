package ids.unicam.models.Service;

import ids.unicam.models.Repository.PoiRepository;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Stato;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class PoiService {
    private final PoiRepository repository;

    @Autowired
    public PoiService(PoiRepository repository) {
        this.repository = repository;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }


    public PuntoInteresse save(Contributor contributor, PuntoInteresse puntoInteresse) {
        if(contributor instanceof ContributorAutorizzato) puntoInteresse.setStato(Stato.APPROVED);//TODO check coordinate
        return repository.save(puntoInteresse);
    }



    public Optional<PuntoInteresse> findById(int id) {
        return repository.findById(id);
    }


    public List<PuntoInteresse> findAll() {
        return repository.findAll();
    }

    public PuntoInteresse getLast() {
        return repository.findAll().getLast();
    }

    public PuntoInteresse getFirst() {
        return repository.findAll().getFirst();
    }


    public void deleteAll() {
        repository.deleteAll();
    }

    public void aggiungiMateriale(PuntoInteresse puntoInteresse, MaterialeGenerico materialeGenerico) {
        //TODO
    }
}
