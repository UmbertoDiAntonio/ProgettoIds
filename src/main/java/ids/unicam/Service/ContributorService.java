package ids.unicam.Service;

import ids.unicam.models.attori.Contributor;

import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

public interface ContributorService {
    Contributor save(Contributor contributor);

    /**
     * Ottieni tutti i Contributor presenti nella piattaforma
     *
     * @return la lista dei contributor presenti
     */
    List<Contributor> getAll();

    /**
     * Elimina il contributor con l'username indicato
     *
     * @param username l'username del Contributor da eliminare
     */
    void deleteByUsername(String username);

    /**
     * Ottieni se esiste il Contributor con l'username indicato
     *
     * @param username l'username da cercare
     * @return il Contributor con l'username cercato, Optional.Empty se non esiste
     */
    Optional<Contributor> getByUsername(String username);

    /**
     * Trova tutti i Contributor nella piattaforma che rispettano la condizione
     *
     * @param predicate un espressione da valutare sui Contributor
     * @return la lista di Contributor che rispettano la condizione
     */
    List<Contributor> find(Predicate<Contributor> predicate);


}
