package ids.unicam.Service;

import ids.unicam.models.attori.Contributor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

public interface ContributorService {
    @NotNull Contributor save(@NotNull Contributor contributor);

    /**
     * Ottieni tutti i Contributor presenti nella piattaforma
     *
     * @return la lista dei contributor presenti
     */
    @NotNull List<Contributor> getAll();

    /**
     * Elimina il contributor con l'username indicato
     *
     * @param username l'username del Contributor da eliminare
     */
    void deleteByUsername(@NotNull String username);

    /**
     * Ottieni se esiste il Contributor con l'username indicato
     *
     * @param username l'username da cercare
     * @return il Contributor con l'username cercato, Optional.Empty se non esiste
     */
    @NotNull Optional<Contributor> getByUsername(@NotNull String username);

    /**
     * Trova tutti i Contributor nella piattaforma che rispettano la condizione
     *
     * @param predicate un espressione da valutare sui Contributor
     * @return la lista di Contributor che rispettano la condizione
     */
    @NotNull List<Contributor> find(@Nullable Predicate<Contributor> predicate);


}
