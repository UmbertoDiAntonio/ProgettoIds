package ids.unicam.Service;

import ids.unicam.models.contenuti.Itinerario;

import java.util.List;
import java.util.Optional;

public interface ItinerarioService {

    boolean aggiungiTappa(String usernameContributor,Integer idItinerario, Integer idPuntoInteresse) throws IllegalArgumentException;

    void aggiungiTappa(String usernameContributor,Integer idItinerario, Integer... idPuntiInteresse) throws IllegalArgumentException;

    Itinerario rimuoviTappa(String usernameContributor,Integer idItinerario, Integer idPuntoInteresse) throws IllegalArgumentException;

    Itinerario creaItinerario(Itinerario itinerario) throws IllegalArgumentException;

    Optional<Itinerario> getById(int id);

    List<Itinerario> getAll();

    void deleteById(int id);

}
