package ids.unicam.Service;

import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Punto;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.DayOfWeek;
import ids.unicam.models.contenuti.puntiInteresse.Orario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.TipologiaPuntoInteresse;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;

public interface PoiService {
    /**
     * Crea un nuovo punto di interesse nella piattaforma
     *
     * @param nomePOI                 nome da dare al nuovo punto di interesse
     * @param punto                   le coordinate del nuovo punto di interesse
     * @param orario                  l'eventuale orario del punto di interesse
     * @param tipologiaPuntoInteresse la tipologia di punto di interesse
     * @param usernameCreatore        l'username del contributor che sta eseguendo l'operazione
     * @return il punto di interesse appena creato
     * @throws FuoriComuneException     se la posizione selezionata è fuori dal territorio in cui può operare il contributor
     * @throws IllegalArgumentException se l'username del contributor non è valido
     */
    @NotNull PuntoInteresse creaPuntoInteresse(@NotNull String nomePOI, @NotNull Punto punto, @NotNull Orario orario, @NotNull TipologiaPuntoInteresse tipologiaPuntoInteresse, @NotNull String usernameCreatore) throws FuoriComuneException, IllegalArgumentException;

    /**
     * Ottieni la data di fine validità di un punto di interesse
     *
     * @param idPunto l'id del punto di interesse
     * @return la data di fine validità
     * @throws IllegalArgumentException se non è stato trovato un punto con l'id indicato
     */
    @NotNull LocalDate getScadenza(int idPunto) throws IllegalArgumentException;

    @Transactional
    @NotNull PuntoInteresse save(@NotNull PuntoInteresse puntoInteresse);

    /**
     * Ottieni il punto di interesse con l'id selezionato, se esiste
     *
     * @param id l'id del punto di interesse da cercare
     * @return il punto di interesse, se trovato, Optional.Empty altrimenti
     */
    @NotNull Optional<PuntoInteresse> findById(int id);

    /**
     * Ottieni i punti di interesse validi presenti nella piattaforma
     *
     * @return la lista dei punti di interesse trovati
     */
    @NotNull List<PuntoInteresse> findActive();

    /**
     * Aggiungi un tag al punto di interesse
     *
     * @param idPuntoInteresse    l'id del punto di interesse a cui aggiungere il tag
     * @param tag                 il tag da aggiungere
     * @param usernameContributor l'username del contributor che sta effettuando l'operazione
     * @throws FuoriComuneException     se il punto di interesse si trova fuori dal territorio in cui può operare il contributor
     * @throws IllegalArgumentException se i parametri non sono corretti
     * @throws IllegalStateException    se il punto di interesse non è valido
     */
    void aggiungiTag(int idPuntoInteresse, @NotNull String tag, @NotNull String usernameContributor) throws FuoriComuneException, IllegalArgumentException, IllegalStateException;

    /**
     * Rimuovi un tag da un punto di interesse
     *
     * @param idPuntoInteresse    l'id del punto di interesse
     * @param tag                 il tag da rimuovere
     * @param usernameContributor l'username del contributor che esegue l'operazione
     * @throws FuoriComuneException     se il punto di interesse si trova fuori dal territorio in cui può operare il contributor
     * @throws IllegalArgumentException se i parametri non sono corretti
     */
    @Transactional
    void rimuoviTag(int idPuntoInteresse, @NotNull String tag, @NotNull String usernameContributor) throws FuoriComuneException;

    /**
     * Trova tutti i Punti di intesse che rispettano la condizione
     *
     * @param predicate la condizione da rispettare
     * @return i punti di interesse trovati
     */
    @NotNull List<PuntoInteresse> find(@Nullable Predicate<PuntoInteresse> predicate);

    /**
     * Ottieni la lista dei tag di un punto di interesse
     *
     * @param idPunto l'id del punto di interesse
     * @return i tag del punto di interesse
     */
    @NotNull List<String> getTags(int idPunto);

    /**
     * Ottieni, se esiste, il punto di interesse con l'id indicato
     *
     * @param id l'id del punto di interesse da cercare
     * @return il punto di interesse, se trovato, Optional.Empty altrimenti
     */
    @NotNull Optional<PuntoInteresse> getById(int id);

    /**
     * Elimina il punto di interesse con l'id indicato
     *
     * @param id l'id del punto di interesse da eliminare
     */
    void deleteById(int id, @NotNull String usernameUtente);

    /**
     * Modifica la data di fine validità di un punto di interesse
     *
     * @param usernameContributor l'username del contributor che esegue l'operazione
     * @param idPuntoInteresse    l'id del punto di interesse
     * @param expireDate          la nuova data di fine validità
     * @throws IllegalArgumentException se i parametri non sono validi
     */
    void modificaScadenza(@NotNull String usernameContributor, int idPuntoInteresse, @NotNull LocalDate expireDate) throws IllegalArgumentException;

    /**
     * Ottieni lo stato del punto di interesse
     *
     * @param idPuntoInteresse l'id del punto di interesse
     * @return lo stato come APPROVATO,NON APPROVATO, IN ATTESA
     */
    @NotNull Stato getStato(int idPuntoInteresse);

    /**
     * Ottieni i materiali associati al punto di interesse
     *
     * @param idPunto l'id del punto di interesse
     * @return i materiali associati al punto di interesse
     * @throws IllegalArgumentException se l'id del punto di interesse non è valido
     */
    @NotNull Set<MaterialeGenerico> getMaterialiPoi(int idPunto) throws IllegalArgumentException;

    /**
     * Ottieni una lista contenente le informazioni generiche di tutti i punti di interesse della piattaforma validi
     *
     * @return la lista di informazioni sui punti di interesse
     */
    @NotNull List<String> getAsList();

    /**
     * Ottieni una lista contenente le informazioni generiche dei punti di interesse forniti
     *
     * @return la lista di informazioni sui punti di interesse
     */
    @NotNull List<String> getAsList(@NotNull List<PuntoInteresse> preferiti);

    /**
     * Ottieni una lista contenente le informazioni dettagliate dei punti di interesse della piattaforma validi
     *
     * @return la lista di informazioni sui punti di interesse
     */
    @Transactional
    @NotNull List<String> getAsListDetailed();

    /**
     * Ottieni tutti i punti di interesse della piattaforma
     *
     * @return i punti di interesse della piattaforma
     */
    @NotNull List<PuntoInteresse> findAll();

    /**
     * Imposta l'orario di apertura di un punto di interesse
     *
     * @param idPunto l'id del punto di interesse
     * @param orario  l'orario da impostare
     * @param day     il giorno in cui impostarlo
     * @throws IllegalArgumentException se l'id del punto di interesse non è valido
     */
    void setOrario(int idPunto, @NotNull Orario.OrarioApertura orario, @NotNull DayOfWeek day);

    /**
     * Se esiste ritorna il punto di interesse che contiene il materiale
     *
     * @param materialeGenerico il materiale di cui volgiamo cercare il contenitore
     * @return ritorna il punto di interesse che contiene il materiale altrimenti Optional.Empty
     */
    @NotNull Optional<PuntoInteresse> getPoiContainingMaterial(@NotNull MaterialeGenerico materialeGenerico);

    /**
     * Elimina il punto di interesse se non è più valido
     *
     * @param puntoInteresse il punto di interesse su cui stiamo eseguendo l'operazione
     */
    void deleteIfIsExpired(@NotNull PuntoInteresse puntoInteresse);

}
