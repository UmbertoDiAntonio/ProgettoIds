package ids.unicam.Service;

import ids.unicam.exception.ContestException;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import jakarta.transaction.Transactional;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface AnimatoreService {


    Animatore save(Animatore animatore);

    /**
     * Elimina l'animatore con l'username indicato
     *
     * @param username l'username dell'animatore da eliminare
     *///TODO rimovere?
    void deleteByUsername(String username);

    /**
     * Ottieni l'animatore con l'username selezionato
     *
     * @param username l'username dell'animatore da cercare
     * @return se esiste ritorna l'animatore con l'username indicato
     */
    Optional<Animatore> getByUsername(String username);

    /**
     * Dichiara un Contest come terminato
     *
     * @param usernameAnimatore username del creatore del contest
     * @param idContest         l'id del contest da terminare
     * @throws UnsupportedOperationException se non si ha il permesso di terminare il contest
     * @throws IllegalArgumentException      se i parametri idContest o usernameAnimatore non sono validi
     */
    void terminaContest(String usernameAnimatore, int idContest) throws UnsupportedOperationException, IllegalArgumentException, ContestException;

    /**
     * Dichiara il vincitore di un contest terminato
     *
     * @param usernameAnimatore username del creatore del contest
     * @param idContest         l'id del contest di cui si vuole dichiarare il vincitore
     * @param idMateriale       l'id del materiale che ha vinto il contest
     * @throws ContestException              se non è possibile dichiarare quel vincitore
     * @throws UnsupportedOperationException se non si ha il permesso di terminare il contest
     * @throws IllegalArgumentException      se i parametri idContest, idMateriale o usernameAnimatore non sono validi
     */
    void setVincitoreContest(String usernameAnimatore, int idContest, int idMateriale) throws ContestException, IllegalArgumentException, UnsupportedOperationException;

    /**
     * Annulla un invito a partecipare a un tuo contest
     *
     * @param usernameAnimatore l'animatore che ha inviato l'invito
     * @param idInvito          l'id dell'invito da annullare
     * @throws IllegalArgumentException      se l'id dell'invito non è corretto o se l'username dell'animatore non esiste
     * @throws UnsupportedOperationException se non hai i permessi per annullare l'invito
     */
    void annullaInvito(String usernameAnimatore, int idInvito) throws ContestException, IllegalArgumentException;

    /**
     * Ottieni tutti gli animatori presenti nella piattaforma
     *
     * @return la lista con tutti gli animatori della piattaforma
     */
    List<Animatore> getAll();

    /**
     * Ottieni tutti gli animatori presenti nel comune selezionato
     *
     * @param nomeComune nome del comune da cercare
     * @return la lista degli animatori associati al comune selezionato
     */
    List<Animatore> findByNomeComune(String nomeComune);

    /**
     * Crea un invito a partecipare a un tuo contest per un utente
     *
     * @param usernameAnimatore username dell'utente che sta invando l'invito
     * @param idContest         il contest per il quale si sta mandando l'invito
     * @param invitato          l'username dell'utente invitato
     * @return l'invito appena creato
     * @throws ContestException         se l'invitato fa già parte del contest
     * @throws IllegalStateException    se non hai i permessi per invitare nel contest
     * @throws IllegalArgumentException se uno dei parametri non è corretto
     */
    Invito invitaContest(String usernameAnimatore, int idContest, String invitato) throws ContestException, IllegalStateException, IllegalArgumentException;

    /**
     * Cambia lo stato di un materiale caricato nel contest, deve essere in "in attesa"
     *
     * @param usernameAnimatore   username dell'animatore che vuole eseguire l'operazione
     * @param idContest           l'id del contest in cui si trova il materiale a cui cambiare lo stato
     * @param idMaterialeGenerico il materiale di cui si vuole cambiare lo stato
     * @param stato               lo stato che si vuole impostare
     * @throws UnsupportedOperationException se non è possibile effettuare l'operazione
     * @throws IllegalArgumentException      se uno dei parametri non è corretto
     */
    void approvaMateriale(String usernameAnimatore, int idContest, int idMaterialeGenerico, boolean stato) throws UnsupportedOperationException, IllegalArgumentException;

    /**
     * Imposta una data per la fine automatica del contest
     *
     * @param idContest         l'id del contest di cui si sta impostando la data di fine
     * @param dataFine          la data da impostare
     * @param usernameAnimatore l'username dell'animatore che esegue l'operazione
     * @throws UnsupportedOperationException se l'animatore non ha i permessi per eseguire l'operazione
     * @throws IllegalArgumentException      se uno dei parametri non è valido
     */
    void setFineContest(int idContest, LocalDate dataFine, String usernameAnimatore) throws UnsupportedOperationException, IllegalArgumentException;

    /**
     * Aggiungi un Tag a un contest
     *
     * @param idContest         l'id del contest in cui si vuole aggiungere il tag
     * @param tag               il tag da aggiungere
     * @param usernameAnimatore l'animatore che sta eseguendo l'operazione
     * @throws ContestException              se il contest è terminato
     * @throws IllegalArgumentException      se uno dei parametri non è valido
     * @throws UnsupportedOperationException se l'animatore non ha i permessi per eseguire questa operazione
     */
    @Transactional
    void aggiungiTagContest(int idContest, String tag, String usernameAnimatore) throws ContestException, IllegalArgumentException, IllegalStateException;

    /**
     * Rimuovi un tag da un contest
     *
     * @param idContest         l'id del contest in cui si vuole rimuovere il tag
     * @param tag               il tag da rimuovere
     * @param usernameAnimatore l'username dell'animatore che vuole eseguire l'operazione
     * @throws ContestException              se il contest è terminato
     * @throws IllegalArgumentException      se uno dei parametri non è valido
     * @throws UnsupportedOperationException se l'animatore non può eseguire l'operazione
     */
    @Transactional
    void rimuoviTagContest(int idContest, String tag, String usernameAnimatore) throws ContestException, IllegalArgumentException;
}
