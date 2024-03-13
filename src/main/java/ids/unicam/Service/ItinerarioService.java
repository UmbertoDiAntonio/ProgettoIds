package ids.unicam.Service;

import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

public interface ItinerarioService {
    Itinerario save(Itinerario itinerario);

    /**
     * Ottieni le tappe dell'itinerario
     *
     * @param idItinerario l'id dell'itinerario di cui si vogliono ottenere le tappe
     * @return la lista di tappe
     * @throws IllegalArgumentException se l'id dell'itinerario non è stato trovato
     */
    List<PuntoInteresse> getTappe(int idItinerario) throws IllegalArgumentException;

    /**
     * Ottieni, se esiste, l'itinerario con il nome indicato
     *
     * @param nome il nome dell'itinerario da cercare
     * @return l'itinerario con il nome indicato, Optional.Empty altrimenti
     */
    Optional<Itinerario> getByNome(String nome);

    /**
     * Ottieni, se esiste, l'itinerario con l'id indicato
     *
     * @param id l'id dell'itinerario da cercare
     * @return l'itinerario con l'id indicato, Optional.Empty altrimenti
     */
    Optional<Itinerario> getById(int id);

    /**
     * Ottieni tutti gli itinerari presenti nella piattaforma
     *
     * @return la lista contenente tutti gli itinerari trovati
     */
    List<Itinerario> getAll();

    /**
     * Elimina l'itinerario con l'id indicato
     *
     * @param id l'id dell'itinerario da eliminare
     */
    void deleteById(int id);

    /**
     * Trova tutti gli Itinerari che rispettano la condizione
     *
     * @param predicate la condizione da rispettare
     * @return gli Itinerari trovati
     */
    List<Itinerario> find(Predicate<Itinerario> predicate);

    /**
     * Aggiungi una tappa all'itinerario
     *
     * @param usernameContributor username del contributor che sta eseguendo l'operazione
     * @param idItinerario        l'id dell'itinerario su cui si vuole aggiungere la tappa
     * @param idPuntoInteresse    l'id del punto di interesse da aggiungere come tappa
     * @throws IllegalArgumentException se i parametri non sono validi
     * @throws FuoriComuneException     se il punto da aggiungere o l'itinerario si trovano fuori dal territorio in cui può operare il contributor
     */
    void aggiungiTappa(String usernameContributor, int idItinerario, int idPuntoInteresse) throws IllegalArgumentException, FuoriComuneException;

    /**
     * Aggiungi una tappa all'itinerario
     *
     * @param usernameContributor username del contributor che sta eseguendo l'operazione
     * @param idItinerario        l'id dell'itinerario su cui si vuole aggiungere la tappa
     * @param idPuntiInteresse    l'id dei punti di interesse da aggiungere come tappe
     * @throws IllegalArgumentException se i parametri non sono validi
     * @throws FuoriComuneException     se i punti da aggiungere o l'itinerario si trovano fuori dal territorio in cui può operare il contributor
     */
    void aggiungiTappa(String usernameContributor, int idItinerario, int... idPuntiInteresse) throws IllegalArgumentException, FuoriComuneException;

    /**
     * Rimuovi una tappa dall'itinerario
     *
     * @param usernameContributor username del contributor che vuole eseguire l'operazione
     * @param idItinerario        l'id dell'itinerario da cui rimuovere la tappa
     * @param idPuntoInteresse    l'id della tappa da rimuovere
     * @throws IllegalArgumentException se uno dei parametri non è valido
     * @throws FuoriComuneException     se l'itinerario si trova fuori dal territorio in cui può operare il contributor
     */
    void rimuoviTappa(String usernameContributor, int idItinerario, int idPuntoInteresse) throws IllegalArgumentException, FuoriComuneException;

    /**
     * Crea un nuovo itinerario
     *
     * @param nomeCreatore   username del contributor che vuole eseguire l'operazione
     * @param nomeItinerario nome da dare all'itinerario
     * @return l'itinerario appena creato
     * @throws IllegalArgumentException se esiste già un itinerario col nome selezionato
     */
    Itinerario creaItinerario(String nomeCreatore, String nomeItinerario) throws IllegalArgumentException;


}
