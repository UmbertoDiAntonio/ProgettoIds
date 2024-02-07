package ids.unicam.models.Service;

import ids.unicam.models.Repository.TuristaAutenticatoRepository;
import ids.unicam.models.attori.TuristaAutenticato;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class TuristaAutenticatoService {

    private final TuristaAutenticatoRepository turistaAutenticatoRepository;

    @Autowired
    public TuristaAutenticatoService(TuristaAutenticatoRepository turistaAutenticatoRepository) {
        this.turistaAutenticatoRepository = turistaAutenticatoRepository;
    }

    public void eliminaTurista(TuristaAutenticato turistaAutenticato) {
        turistaAutenticatoRepository.deleteById(turistaAutenticato.getId());
    }

    public TuristaAutenticato salvaTurista(TuristaAutenticato turistaAutenticato) {
        turistaAutenticato = turistaAutenticatoRepository.save(turistaAutenticato);
        return turistaAutenticato;
    }

    public TuristaAutenticato cercaTurista(TuristaAutenticato turistaAutenticato) {
        return cercaTurista(turistaAutenticato.getId());
    }

    public TuristaAutenticato cercaTurista(int id) {
        Optional<TuristaAutenticato> ricerca = turistaAutenticatoRepository.findById(id);
        if (ricerca.isPresent())
            return ricerca.get();
        else {
            logger.warn("Turista non trovato da ID");
            return null;
        }
    }

    public Iterable<TuristaAutenticato> elencoTuristi() {
        return turistaAutenticatoRepository.findAll();
    }

    public TuristaAutenticato getLast(){
        return turistaAutenticatoRepository.getLast();
    }

    public TuristaAutenticato getFirst(){
        return turistaAutenticatoRepository.getFirst();
    }


    public void eliminaListaTuristi() {
        turistaAutenticatoRepository.deleteAll();
    }


}
