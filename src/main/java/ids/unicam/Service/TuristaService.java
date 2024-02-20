package ids.unicam.Service;

import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.Tag;

import java.util.List;
import java.util.Optional;

public interface TuristaService {
    List<Taggable> findByTag(Tag tag);

    void report(PuntoInteresse puntoInteresse);


    Optional<TuristaAutenticato> accedi(String username, String password);
}
