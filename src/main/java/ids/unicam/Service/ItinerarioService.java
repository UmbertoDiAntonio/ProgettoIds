package ids.unicam.Service;

import ids.unicam.models.contenuti.Itinerario;

import java.util.List;
import java.util.Optional;

public interface ItinerarioService {

    boolean aggiungiTappa(String usernameContributor,Integer idItinerario, Integer idPuntoInteresse);

    void aggiungiTappa(String usernameContributor,Integer idItinerario, Integer... idPuntiInteresse);

    Itinerario rimuoviTappa(String usernameContributor,Integer idItinerario, Integer idPuntoInteresse);

    Itinerario creaItinerario(Itinerario itinerario);

    Optional<Itinerario> getById(int id);

    List<Itinerario> getAll();

    void deleteById(int id);

}
