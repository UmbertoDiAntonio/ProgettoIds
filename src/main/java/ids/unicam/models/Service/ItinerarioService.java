package ids.unicam.models.Service;

import ids.unicam.Comune;
import ids.unicam.models.Repository.ItinerarioRepository;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.PuntoInteresse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class ItinerarioService {
    private final ItinerarioRepository repository;

    @Autowired
    public ItinerarioService(ItinerarioRepository repository) {
        this.repository = repository;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }


    public Itinerario save(Itinerario itinerario) {
        return repository.save(itinerario);
    }


    public boolean aggiungiTappa(Itinerario itinerario, PuntoInteresse puntoInteresse){
        itinerario.getPercorso().add(puntoInteresse);//TODO non è la soluzione, va validato che sia nel comune, e poi aggiunto ma non così
        return true;
    }
    public void aggiungiTappa(Itinerario itinerario, PuntoInteresse... puntiInteresse){
        for(PuntoInteresse puntoInteresse:puntiInteresse)
            aggiungiTappa(itinerario,puntoInteresse);
    }
    public void rimuoviTappa(Itinerario itinerario,PuntoInteresse puntoInteresse){
        //TODo
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
}
