package ids.unicam.Service;

import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.contenuti.Itinerario;

import java.util.List;
import java.util.Optional;

public interface ItinerarioService {

    boolean aggiungiTappa(String usernameContributor,Integer idItinerario, Integer idPuntoInteresse) throws IllegalArgumentException, FuoriComuneException;

    void aggiungiTappa(String usernameContributor,Integer idItinerario, Integer... idPuntiInteresse) throws IllegalArgumentException, FuoriComuneException;

    Itinerario rimuoviTappa(String usernameContributor,Integer idItinerario, Integer idPuntoInteresse) throws IllegalArgumentException;

    Itinerario creaItinerario(String nomeCreatore, String nomeItinerario) throws IllegalArgumentException;

    Optional<Itinerario> getById(int id);

    List<Itinerario> getAll();

    void deleteById(int id);

}
