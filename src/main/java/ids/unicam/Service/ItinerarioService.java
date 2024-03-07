package ids.unicam.Service;

import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.contenuti.Itinerario;

import java.util.List;
import java.util.Optional;

public interface ItinerarioService {

    boolean aggiungiTappa(String usernameContributor, int idItinerario, int idPuntoInteresse) throws IllegalArgumentException, FuoriComuneException;

    void aggiungiTappa(String usernameContributor, int idItinerario, int... idPuntiInteresse) throws IllegalArgumentException, FuoriComuneException;

    void rimuoviTappa(String usernameContributor, int idItinerario, int idPuntoInteresse) throws IllegalArgumentException, FuoriComuneException;

    Itinerario creaItinerario(String nomeCreatore, String nomeItinerario) throws IllegalArgumentException;

    Optional<Itinerario> getById(int id);

    List<Itinerario> getAll();

    void deleteById(int id);

    List<Itinerario> findByNomeComune(String nomeComune);
}
