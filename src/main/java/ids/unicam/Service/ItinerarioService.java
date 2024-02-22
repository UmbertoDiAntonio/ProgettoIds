package ids.unicam.Service;

import ids.unicam.models.attori.Contributor;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import io.swagger.models.auth.In;

import java.util.List;
import java.util.Optional;

public interface ItinerarioService {

    boolean aggiungiTappa(String usernameContributor,Integer idItinerario, Integer idPuntoInteresse);

    void aggiungiTappa(String usernameContributor,Integer idItinerario, Integer... idPuntiInteresse);

    Itinerario rimuoviTappa(Itinerario itinerario, PuntoInteresse puntoInteresse);

    Itinerario creaItinerario(Itinerario itinerario);

    Optional<Itinerario> getById(int id);

    List<Itinerario> getAll();

    void deleteById(int id);

    Itinerario update(Itinerario itinerario, int id);

}
