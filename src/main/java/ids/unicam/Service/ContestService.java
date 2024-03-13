package ids.unicam.Service;

import ids.unicam.exception.ContestException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import jakarta.transaction.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

public interface ContestService {
    /**
     * Aggiungi un materiale al contest
     * @param usernameTurista l'utente che vuole cambiare il materiale
     * @param idContest l'id del contest in cui caricare il materiale
     * @param materialeGenerico il materiale da caricare
     * @throws ContestException se non sei inscritto al contest o il contest è terminato
     * @throws IllegalArgumentException se i parametri non sono validi
     */
    void aggiungiMateriale(String usernameTurista, int idContest, MaterialeGenerico materialeGenerico) throws ContestException, FuoriComuneException;
    /**
     * Ottieni la lista dei partecipanti del contest
     * @param contest il contest di cui si vogliono ottenere i partecipanti
     * @return la lista dei partecipanti del contest
     */
    List<TuristaAutenticato> getPartecipanti(Contest contest);
    /**
     * Crea un nuovo contest
     * @param nomeContest il nome da dare al contest
     * @param obiettivo l'obiettivo del contest
     * @param creatore il creatore del contest
     * @param open la modalità di ingresso al contest, aperto o su invito
     * @return il cotest appena creato
     */
    Contest creaContest(String nomeContest, String obiettivo, Animatore creatore, boolean open);

    /**
     * Trova tutti i Contest che rispettano la condizione
     * @param predicate la condizione da rispettare
     * @return i contest trovati
     */
    List<Taggable> find(Predicate<Contest> predicate);
    /**
     * Ottieni tutti i materiali del contest
     * @param contest il contest di cui si vogliono ottenere i materiali
     * @return la lista dei materiali del contest
     */
    List<MaterialeGenerico> getMaterialiContest(Contest contest);

    void setVincitoreContest(Contest contest, MaterialeGenerico materialeGenerico) throws ContestException;

    void terminaContest(Contest contest);

    List<Contest> findAll();

    Contest save(Contest contest);

    Optional<Contest> findById(int id);

    void deleteById(int id);

    Optional<Contest> getContestContainingMaterial(MaterialeGenerico materialeGenerico);

    @Transactional
    void checkIfIsExpired(Contest contest);

    List<Contest> getContestByComune(String nomeComune);
}
