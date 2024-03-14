package ids.unicam.Service;

import ids.unicam.exception.ContestException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Optional;

public interface TuristaAutenticatoService {

    @NotNull TuristaAutenticato save(@NotNull TuristaAutenticato turistaAutenticato);

    /**
     * Accetta un invito a partecipare a un contest
     *
     * @param usernameUtente l'username dell' Utente che esegue l'operazione
     * @param idInvito       l'id dell'invito da accettare
     * @throws IllegalArgumentException se i parametri non sono corretti
     * @throws ContestException         se non è possibile accettare l'invito
     */
    @Transactional
    void accettaInvitoContest(@NotNull String usernameUtente, int idInvito) throws IllegalArgumentException, ContestException;

    /**
     * Rimuovi un punto di interesse dalla lista dei preferiti
     *
     * @param usernameTurista l'utente che sta eseguendo l'operazione
     * @param idPunto         l'id del punto di interesse
     * @throws IllegalArgumentException se i parametri non sono corretti
     */
    void rimuoviPreferito(@NotNull String usernameTurista, int idPunto) throws IllegalArgumentException;

    /**
     * Carica un materiale in un Contenitore
     *
     * @param usernameTurista   l'username dell'utente che sta eseguendo l'operazione
     * @param idContenitore     l'id del contenitore
     * @param materialeGenerico il materiale da caricare
     * @throws IllegalArgumentException se i parametri non sono validi
     * @throws IllegalStateException    se il contenitore è un punto di interesse e non è approvato
     * @throws ContestException         se il contest è terminato o richiede iscrizione e non sei inscritto
     * @throws FuoriComuneException     se il contenitore si trova fuori dal territorio in cui può operare l'utente
     */
    @Transactional
    void aggiungiMateriale(@NotNull String usernameTurista, int idContenitore, @NotNull MaterialeGenerico materialeGenerico) throws IllegalArgumentException, IllegalStateException, ContestException, FuoriComuneException;

    /**
     * Aggiungi un punto di interesse ai preferiti
     *
     * @param usernameTurista  l'username dell'utente che sta eseguendo l'operazione
     * @param idPuntoInteresse l'id del punto di interesse
     * @throws IllegalArgumentException se i parametri non sono validi
     */
    void aggiungiPreferito(@NotNull String usernameTurista, int idPuntoInteresse) throws IllegalArgumentException;

    /**
     * Ottieni la lista dei punti di interesse preferiti di un utente
     *
     * @param usernameTurista l'username dell'utente che esegue l'operazione
     * @return la lista dei punti di interesse preferiti dell'utente
     * @throws IllegalArgumentException se l'username dell'utente non viene trovato
     */
    @NotNull List<PuntoInteresse> findPreferiti(@NotNull String usernameTurista) throws IllegalArgumentException;

    /**
     * Partecipa a un contest
     *
     * @param idContest       l'id del contest
     * @param usernameTurista l'username dell'utente che esegue l'operazione
     * @throws IllegalArgumentException se i parametri non sono corretti
     * @throws ContestException         se il contest richiede invito
     */
    void partecipaAlContest(int idContest, @NotNull String usernameTurista) throws UnsupportedOperationException, IllegalArgumentException, ContestException;

    /**
     * Smetti di partecipare a un contest
     *
     * @param idContest       l'id del contest
     * @param usernameTurista l'username dell'utente che sta eseguendo l'operazione
     * @throws IllegalArgumentException se i parametri non sono corretti
     */
    @Transactional
    void cancellaPartecipazioneContest(int idContest, @NotNull String usernameTurista) throws IllegalArgumentException;

    /**
     * Verifica se un username è unico
     *
     * @param username l'username da controllare
     * @return true se è unico, false altrimenti
     */
    boolean isUsernameUnique(@NotNull String username);

    /**
     * Ottieni la lista di tutti i Turisti Autenticati presenti nella piattaforma
     *
     * @return la lista dei Turisti Autenticati
     */
    @NotNull List<TuristaAutenticato> getAll();

    /**
     * Ottieni, se esiste, il turista autenticato con l'username indicato
     *
     * @param username l'username cercato
     * @return il turista con l'username cercato, se esiste, Optional.Empty altrimenti
     */
    @NotNull Optional<TuristaAutenticato> getByUsername(@NotNull String username);

    /**
     * Elimina il Turista Autenticato con l'username indicato
     *
     * @param username l'username cercato
     */
    void deleteByUsername(@NotNull String username);

    /**
     * Ottieni la lista delle notifiche ricevute dall'utente
     *
     * @param usernameTurista l'username dell'utente che vuole eseguire l'operazione
     * @return la lista delle notifiche
     * @throws IllegalArgumentException se l'username non è corretto
     */
    @NotNull List<Notifica> visualizzaNotifiche(@NotNull String usernameTurista) throws IllegalArgumentException;

    /**
     * Ottieni la lista degli inviti ricevi da un utente
     *
     * @param usernameTurista l'utente che sta eseguendo l'operazione
     * @return la lista degli inviti trovati
     */
    @NotNull List<Invito> getInviti(@NotNull String usernameTurista);

    /**
     * Elimina tutte le notifiche dell'utente
     *
     * @param usernameTurista l'utente che esegue l'operazione
     */
    void deleteNotificheByUsername(@NotNull String usernameTurista);
}
