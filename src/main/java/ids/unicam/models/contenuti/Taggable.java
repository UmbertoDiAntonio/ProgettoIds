package ids.unicam.models.contenuti;

import java.util.List;
import java.util.Set;

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
    void addTag(String tag);

    void rimuoviTag(String tag);
}
