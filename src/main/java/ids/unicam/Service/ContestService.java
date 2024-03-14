package ids.unicam.Service;

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
     * Ottieni la lista dei partecipanti del contest
     *
     * @param contest il contest di cui si vogliono ottenere i partecipanti
     * @return la lista dei partecipanti del contest
     */
    List<TuristaAutenticato> getPartecipanti(Contest contest);

    /**
     * Crea un nuovo contest
     *
     * @param nomeContest il nome da dare al contest
     * @param obiettivo   l'obiettivo del contest
     * @param creatore    il creatore del contest
     * @param open        la modalità di ingresso al contest, aperto o su invito
     * @return il cotest appena creato
     */
    Contest creaContest(String nomeContest, String obiettivo, Animatore creatore, boolean open);

    /**
     * Trova tutti i Contest che rispettano la condizione
     *
     * @param predicate la condizione da rispettare
     * @return i contest trovati
     */
    List<Taggable> find(Predicate<Contest> predicate);

    /**
     * Ottieni tutti i materiali del contest
     *
     * @param contest il contest di cui si vogliono ottenere i materiali
     * @return la lista dei materiali del contest
     */
    List<MaterialeGenerico> getMaterialiContest(Contest contest);

    /**
     * Termina un Contest, successivamente l'animatore dovrà decretare il vincitore
     *
     * @param contest il contest da terminare
     */
    void terminaContest(Contest contest);

    /**
     * Ottieni tutti i Contest presenti nella piattaforma
     *
     * @return la lista dei contest presenti sulla piattaforma
     */
    List<Contest> findAll();

    /**
     * Ottieni il contest che ha come id l'id indicato
     *
     * @param id l'id del contest che stiamo cercando
     * @return se esiste ritorna il contest cercato
     */

    Optional<Contest> findById(int id);

    void deleteById(int id);

    Contest save(Contest contest);

    /**
     * Se esiste ritorna il contest che contiene il materiale
     *
     * @param materialeGenerico il materiale di cui volgiamo cercare il contenitore
     * @return ritorna il contest che contiene il materiale altrimenti Optional.Empty
     */
    Optional<Contest> getContestContainingMaterial(MaterialeGenerico materialeGenerico);

    /**
     * Controlla se la validità del contest è terminata e nel caso lo termina
     *
     * @param contest il contest che si sta controllando
     */
    @Transactional
    void checkIfIsExpired(Contest contest);

    /**
     * Trova tutti i contest nella piattaforma che rispettano la condizione
     *
     * @param predicate un espressione da valutare sui Contest
     * @return la lista di contest che rispettano la condizione
     */
    List<Contest> getContest(Predicate<Contest> predicate);
}
