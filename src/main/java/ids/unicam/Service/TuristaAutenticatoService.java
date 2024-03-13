package ids.unicam.Service;

import ids.unicam.exception.ContestException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;

import java.util.List;
import java.util.Optional;

public interface TuristaAutenticatoService {

    TuristaAutenticato save(TuristaAutenticato turistaAutenticato);

    /**
     * Accetta un invito a partecipare a un contest
     *
     * @param usernameUtente l'username dell' Utente che esegue l'operazione
     * @param idInvito       l'id dell'invito da accettare
     * @throws IllegalArgumentException se i parametri non sono corretti
     * @throws ContestException         se non è possibile accettare l'invito
     */
    @Transactional
    void accettaInvitoContest(String usernameUtente, int idInvito) throws IllegalArgumentException, ContestException;

    /**
     * Rimuovi un punto di interesse dalla lista dei preferiti
     *
     * @param usernameTurista l'utente che sta eseguendo l'operazione
     * @param idPunto         l'id del punto di interesse
     * @throws IllegalArgumentException se i parametri non sono corretti
     */
    void rimuoviPreferito(String usernameTurista, int idPunto) throws IllegalArgumentException;

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
    void aggiungiMateriale(String usernameTurista, int idContenitore, MaterialeGenerico materialeGenerico) throws IllegalArgumentException, IllegalStateException, ContestException, FuoriComuneException;

    /**
     * Aggiungi un punto di interesse ai preferiti
     *
     * @param usernameTurista  l'username dell'utente che sta eseguendo l'operazione
     * @param idPuntoInteresse l'id del punto di interesse
     * @throws IllegalArgumentException se i parametri non sono validi
     */
    void aggiungiPreferito(String usernameTurista, int idPuntoInteresse) throws IllegalArgumentException;

    /**
     * Ottieni la lista dei punti di interesse preferiti di un utente
     *
     * @param usernameTurista l'username dell'utente che esegue l'operazione
     * @return la lista dei punti di interesse preferiti dell'utente
     * @throws IllegalArgumentException se l'username dell'utente non viene trovato
     */
    List<PuntoInteresse> findPreferiti(String usernameTurista) throws IllegalArgumentException;

    /**
     * Partecipa a un contest
     *
     * @param idContest       l'id del contest
     * @param usernameTurista l'username dell'utente che esegue l'operazione
     * @throws IllegalArgumentException se i parametri non sono corretti
     * @throws ContestException         se il contest richiede invito
     */
    void partecipaAlContest(int idContest, String usernameTurista) throws UnsupportedOperationException, IllegalArgumentException, ContestException;

    /**
     * Smetti di partecipare a un contest
     *
     * @param idContest       l'id del contest
     * @param usernameTurista l'username dell'utente che sta eseguendo l'operazione
     * @throws IllegalArgumentException se i parametri non sono corretti
     */
    @Transactional
    void cancellaPartecipazioneContest(int idContest, String usernameTurista) throws IllegalArgumentException;

    /**
     * Verifica se un username è unico
     *
     * @param username l'username da controllare
     * @return true se è unico, false altrimenti
     */
    boolean isUsernameUnique(String username);

    /**
     * Ottieni la lista di tutti i Turisti Autenticati presenti nella piattaforma
     *
     * @return la lista dei Turisti Autenticati
     */
    List<TuristaAutenticato> getAll();

    /**
     * Ottieni, se esiste, il turista autenticato con l'username indicato
     *
     * @param username l'username cercato
     * @return il turista con l'username cercato, se esiste, Optional.Empty altrimenti
     */
    Optional<TuristaAutenticato> getByUsername(String username);

    /**
     * Elimina il Turista Autenticato con l'username indicato
     *
     * @param username l'username cercato
     */
    void deleteByUsername(String username);

    /**
     * Ottieni la lista delle notifiche ricevute dall'utente
     *
     * @param usernameTurista l'username dell'utente che vuole eseguire l'operazione
     * @return la lista delle notifiche
     * @throws IllegalArgumentException se l'username non è corretto
     */
    List<Notifica> visualizzaNotifiche(String usernameTurista) throws IllegalArgumentException;

    /**
     * Ottieni la lista degli inviti ricevi da un utente
     *
     * @param usernameTurista l'utente che sta eseguendo l'operazione
     * @return la lista degli inviti trovati
     */
    List<Invito> getInviti(String usernameTurista);

    /**
     * Elimina tutte le notifiche dell'utente
     *
     * @param usernameTurista l'utente che esegue l'operazione
     */
    void deleteNotificheByUsername(String usernameTurista);
}
