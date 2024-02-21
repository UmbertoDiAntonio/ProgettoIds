package ids.unicam.Service;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

import java.util.List;
import java.util.Optional;

public interface ItinerarioService {

    boolean aggiungiTappa(Itinerario itinerario, PuntoInteresse puntoInteresse);

    void aggiungiTappa(Itinerario itinerario, PuntoInteresse... puntiInteresse);

    void rimuoviTappa(Itinerario itinerario, PuntoInteresse puntoInteresse);

    Itinerario creaItinerario(Itinerario itinerario);

    Optional<Itinerario> getById(int id);

    List<Itinerario> getAll();

    void deleteById(int id);

    Itinerario update(Itinerario itinerario, int id);

}
