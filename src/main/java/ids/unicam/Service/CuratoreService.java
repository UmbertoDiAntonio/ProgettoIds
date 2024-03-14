package ids.unicam.Service;

import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.Stato;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

public interface CuratoreService {
    @NotNull Curatore save(@NotNull Curatore curatore);

    /**
     * Ottieni tutti i Curatori presenti nella piattaforma
     *
     * @return la lista dei Curatori presenti
     */
    @NotNull List<Curatore> getAll();

    /**
     * Elimina il Curatore con l'username indicato
     *
     * @param username l'username del Curatore da eliminare
     */
    void deleteByUsername(@NotNull String username);

    /**
     * Ottieni se esiste il Curatore con l'username indicato
     *
     * @param username l'username da cercare
     * @return il Curatore con l'username cercato, Optional.Empty se non esiste
     */
    @NotNull Optional<Curatore> getByUsername(@NotNull String username);

    /**
     * Trova tutti i Curatori nella piattaforma che rispettano la condizione
     *
     * @param predicate un espressione da valutare sui Contributor
     * @return la lista di Curatori che rispettano la condizione
     */
    @NotNull List<Curatore> find(@Nullable Predicate<Curatore> predicate);

    /**
     * Cambia lo stato di un Punto di Interesse
     *
     * @param usernameCuratore l'username del curatore che sta eseguendo l'operazione
     * @param idPuntoInteresse l'id del punto di interesse di cui si vuole cambiare lo stato
     * @param stato            lo stato che si vuole impostare (true = Approvato, false= Non Approvato)
     * @throws IllegalArgumentException      Se i parametri non sono corretti
     * @throws UnsupportedOperationException se il punto di interesse ha già uno stato o se si sta impostando uno stato non valido
     * @throws FuoriComuneException          se il punto di interesse si trova in un comune diverso da quello in cui il contributor può operare
     * @see Stato
     */
    void valutaPuntoInteresse(@NotNull String usernameCuratore, int idPuntoInteresse, @Nullable Boolean stato) throws IllegalArgumentException, UnsupportedOperationException, FuoriComuneException;

    /**
     * Cambia lo stato di un materiale
     *
     * @param usernameCuratore    l'username del curatore che sta eseguendo l'operazione
     * @param idMaterialeGenerico l'id del materiale
     * @param stato               lo Stato da impostare
     * @throws IllegalArgumentException      se uno dei parametri non è valido
     * @throws UnsupportedOperationException se il materiale ha già uno stato impostato
     * @throws FuoriComuneException          se il materiale è caricato su un contenitore che si trova fuori dal territorio in cui può operare il curatore
     */
    void valutaMateriale(String usernameCuratore, int idMaterialeGenerico, @Nullable Boolean stato) throws IllegalArgumentException, UnsupportedOperationException, FuoriComuneException;

    /**
     * Elimina un punto di interesse dalla piattaforma
     *
     * @param usernameCuratore l'username del curatore che vuole eseguire l'operazione
     * @param idPuntoInteresse l'id del punto di interesse da eliminare
     * @throws IllegalArgumentException se uno dei parametri non è corretto
     * @throws FuoriComuneException     se il punto di interesse si trova fuori dal territorio in cui può operare il curatore
     */
    void eliminaPuntoInteresse(@NotNull String usernameCuratore, int idPuntoInteresse) throws IllegalArgumentException, FuoriComuneException;

    /**
     * Elimina un itinerario dalla piattaforma
     *
     * @param usernameCuratore l'username del curatore che vuole eseguire l'operazione
     * @param idItinerario     l'id dell'itinerario da eliminare
     * @throws IllegalArgumentException se uno dei parametri non è corretto
     * @throws FuoriComuneException     se l'itinerario si trova fuori dal territorio in cui può operare il curatore
     */
    void eliminaItinerario(@NotNull String usernameCuratore, int idItinerario) throws IllegalArgumentException, FuoriComuneException;

    /**
     * Elimina un contest dalla piattaforma
     *
     * @param usernameCuratore l'username del curatore che vuole eseguire l'operazione
     * @param idContest        l'id del contest da eliminare
     * @throws IllegalArgumentException se uno dei parametri non è corretto
     * @throws FuoriComuneException     se il contest si trova fuori dal territorio in cui può operare il curatore
     */
    void eliminaContest(@NotNull String usernameCuratore, int idContest) throws IllegalArgumentException, FuoriComuneException;

    /**
     * Elimina un materiale dalla piattaforma
     *
     * @param usernameCuratore l'username del curatore che vuole eseguire l'operazione
     * @param idMateriale      l'id del materiale da eliminare
     * @throws IllegalArgumentException se uno dei parametri non è corretto
     * @throws FuoriComuneException     se il materiale si trova fuori dal territorio in cui può operare il curatore
     */
    void eliminaMateriale(@NotNull String usernameCuratore, int idMateriale) throws IllegalArgumentException, FuoriComuneException;
}
