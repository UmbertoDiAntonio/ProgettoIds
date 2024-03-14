package ids.unicam.Service;

import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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
    @NotNull List<TuristaAutenticato> getPartecipanti(Contest contest);

    /**
     * Crea un nuovo contest
     *
     * @param nomeContest il nome da dare al contest
     * @param obiettivo   l'obiettivo del contest
     * @param creatore    il creatore del contest
     * @param open        la modalità di ingresso al contest, aperto o su invito
     * @return il cotest appena creato
     */
    @NotNull Contest creaContest(@NotNull String nomeContest, @NotNull String obiettivo, @NotNull Animatore creatore, boolean open);

    /**
     * Trova tutti i Contest che rispettano la condizione
     *
     * @param predicate la condizione da rispettare
     * @return i contest trovati
     */
    @NotNull List<Taggable> find(@Nullable Predicate<Contest> predicate);

    /**
     * Ottieni tutti i materiali del contest
     *
     * @param contest il contest di cui si vogliono ottenere i materiali
     * @return la lista dei materiali del contest
     */
    @NotNull List<MaterialeGenerico> getMaterialiContest(@NotNull Contest contest);

    /**
     * Termina un Contest, successivamente l'animatore dovrà decretare il vincitore
     *
     * @param contest il contest da terminare
     */
    void terminaContest(@NotNull Contest contest);

    /**
     * Ottieni tutti i Contest presenti nella piattaforma
     *
     * @return la lista dei contest presenti sulla piattaforma
     */
    @NotNull List<Contest> findAll();

    /**
     * Ottieni il contest che ha come id l'id indicato
     *
     * @param id l'id del contest che stiamo cercando
     * @return se esiste ritorna il contest cercato
     */

    @NotNull Optional<Contest> findById(int id);

    void deleteById(int id, @NotNull String usernameUtente);

    @NotNull Contest save(@NotNull Contest contest);

    /**
     * Se esiste ritorna il contest che contiene il materiale
     *
     * @param materialeGenerico il materiale di cui volgiamo cercare il contenitore
     * @return ritorna il contest che contiene il materiale altrimenti Optional.Empty
     */
    @NotNull Optional<Contest> getContestContainingMaterial(@NotNull MaterialeGenerico materialeGenerico);

    /**
     * Controlla se la validità del contest è terminata e nel caso lo termina
     *
     * @param contest il contest che si sta controllando
     */
    @Transactional
    void checkIfIsExpired(@NotNull Contest contest);

    /**
     * Trova tutti i contest nella piattaforma che rispettano la condizione
     *
     * @param predicate un espressione da valutare sui Contest
     * @return la lista di contest che rispettano la condizione
     */
    @NotNull List<Contest> getContest(@Nullable Predicate<Contest> predicate);
}
