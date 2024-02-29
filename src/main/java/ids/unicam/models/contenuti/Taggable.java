package ids.unicam.models.contenuti;

import ids.unicam.models.contenuti.puntiInteresse.Tag;

import java.util.Set;

/**
 * Interfaccia usata da oggetti che possono avere dei Tag
 */
public interface Taggable {
    /**
     * @return tutti i tag dell'oggetto
     */
    Set<Tag> getTags();

    /**
     * @param tag Tag da aggiungere all'oggetto
     */
    void addTag(Tag tag);
}
