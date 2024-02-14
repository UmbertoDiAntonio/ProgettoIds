package ids.unicam.models.Service;

import ids.unicam.Comune;
import ids.unicam.models.Repository.ItinerarioRepository;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@Transactional
public class ItinerarioService {
    private final ItinerarioRepository repository;
    private final PoiService poiService;
    @Autowired
    public ItinerarioService(ItinerarioRepository repository, PoiService poiService) {
        this.repository = repository;
        this.poiService = poiService;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }


    public Itinerario save(Itinerario itinerario) {
        return repository.save(itinerario);
    }

@Transactional
    public boolean aggiungiTappa(Itinerario itinerario, PuntoInteresse puntoInteresse){
        if(itinerario.getComune().equals(puntoInteresse.getComune())){
            poiService.save(puntoInteresse);
            itinerario.aggiungiTappaPercorso(puntoInteresse);//TODO non si aggiunge al database
            save(itinerario);
            return true;
        }
        return false;
    }
    public void aggiungiTappa(Itinerario itinerario, PuntoInteresse... puntiInteresse){
        for(PuntoInteresse puntoInteresse:puntiInteresse) {
            aggiungiTappa(itinerario, puntoInteresse);
        }
    }
    public void rimuoviTappa(Itinerario itinerario,PuntoInteresse puntoInteresse){
        itinerario.getPercorso().remove(puntoInteresse);
        save(itinerario);
    }

    public Optional<Itinerario> findById(int id) {
        return repository.findById(id);
    }


    public List<Itinerario> findAll() {
        return repository.findAll();
    }

    public Itinerario getLast() {
        return repository.findAll().getLast();
    }

    public Itinerario getFirst() {
        return repository.findAll().getFirst();
    }


    public void deleteAll() {
        repository.deleteAll();
    }

    public List<Itinerario> findAllByComune(Comune comune) {
        return repository.findAllByComune(comune);
    }

    public int getNumeroTappe(Itinerario itinerario) {
        return repository.countNumeroTappeItinerario(itinerario.getId());
    }
}
