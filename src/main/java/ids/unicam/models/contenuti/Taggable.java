package ids.unicam.models.contenuti;

import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * Interfaccia usata da oggetti che possono avere dei Tag
 */
public interface Taggable {
    /**
     * @return tutti i tag dell'oggetto
     */
    List<String> getTags();

    /**
     * @param tag Tag da aggiungere all'oggetto
     */
    void addTag( @NotNull String tag);

    /**
     * @param tag tag da rimuovere all'oggetto
     */
    void rimuoviTag( @NotNull String tag);

    /**
     * Controlla se l'oggetto ha il tag indicato
     *
     * @param tag il tag da controllare
     * @return true se l'oggetto contiene il tag, false altrimenti
     */
    boolean haveTag( @NotNull String tag);
}
