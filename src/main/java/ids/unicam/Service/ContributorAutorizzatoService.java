package ids.unicam.Service;

import ids.unicam.models.attori.ContributorAutorizzato;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

public interface ContributorAutorizzatoService {
    @NotNull ContributorAutorizzato save(@NotNull ContributorAutorizzato contributorAutorizzato);

    /**
     * Ottieni tutti i Contributor Autorizzati presenti nella piattaforma
     *
     * @return la lista dei contributor autorizzati presenti
     */
    @NotNull List<ContributorAutorizzato> getAll();

    /**
     * Elimina il contributor autorizzato con l'username indicato
     *
     * @param username l'username del Contributor Autorizzato da eliminare
     */
    void deleteByUsername(String username);

    /**
     * Ottieni se esiste il Contributor Autorizzato con l'username indicato
     *
     * @param username l'username da cercare
     * @return il Contributor Autorizzato con l'username cercato, Optional.Empty se non esiste
     */
    @NotNull Optional<ContributorAutorizzato> getByUsername(@NotNull String username);

    /**
     * Trova tutti i Contributor Autorizzati nella piattaforma che rispettano la condizione
     *
     * @param predicate un espressione da valutare sui Contributor Autorizzati
     * @return la lista di Contributor Autorizzati che rispettano la condizione
     */
    @NotNull List<ContributorAutorizzato> find(@Nullable Predicate<ContributorAutorizzato> predicate);
}
