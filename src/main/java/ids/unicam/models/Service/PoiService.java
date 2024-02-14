package ids.unicam.models.Service;

import ids.unicam.models.Repository.PoiRepository;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Iterator;
import java.util.List;
import java.util.Optional;

@Service
public class PoiService {
    private final PoiRepository repository;
    private final TuristaAutenticatoService turistaAutenticatoService;

    @Autowired
    public PoiService(PoiRepository repository, TuristaAutenticatoService turistaAutenticatoService) {
        this.repository = repository;
        this.turistaAutenticatoService = turistaAutenticatoService;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }

    @Transactional
    public void eliminaPuntoInteresse(int idPuntoInteresse) {
        // Elimina il PuntoInteresse dal database
        repository.deleteById(idPuntoInteresse);

        // Rileva l'eliminazione e aggiorna le liste di preferiti dei turisti
        List<TuristaAutenticato> turisti = turistaAutenticatoService.findTuristiConPreferiti();
       for (TuristaAutenticato turista : turisti) {
            turistaAutenticatoService.rimuoviPreferito(turista, idPuntoInteresse);

        }
    }


    public PuntoInteresse save(PuntoInteresse puntoInteresse) {
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


}
