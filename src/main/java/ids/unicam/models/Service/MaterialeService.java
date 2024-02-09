package ids.unicam.models.Service;

import ids.unicam.models.Repository.ItinerarioRepository;
import ids.unicam.models.Repository.MaterialeRepository;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Stato;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class MaterialeService {
    private final MaterialeRepository repository;
    private final PoiService poiService;

    @Autowired
    public MaterialeService(MaterialeRepository repository, PoiService poiService) {
        this.repository = repository;
        this.poiService = poiService;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }


    public MaterialeGenerico save(Contributor contributor, PuntoInteresse puntoInteresse,MaterialeGenerico materialeGenerico) {
        if(contributor instanceof ContributorAutorizzato) materialeGenerico.setStato(Stato.APPROVED);
        poiService.aggiungiMateriale(puntoInteresse,materialeGenerico);//TODO check se si può aggiungere
        return repository.save(materialeGenerico);
    }


    public void aggiungiMateriale(PuntoInteresse puntoInteresse, MaterialeGenerico materialeGenerico){
        poiService.aggiungiMateriale(puntoInteresse,materialeGenerico);
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

}
