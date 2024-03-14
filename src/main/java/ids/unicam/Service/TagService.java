package ids.unicam.Service;

import ids.unicam.models.contenuti.Taggable;
import org.jetbrains.annotations.NotNull;

public interface TagService {
    /**
     * Aggiungi un tag all'oggetto
     *
     * @param taggableObject l'oggetto a cui aggiungere il tag
     * @param tag            il tag da aggiungere
     */
    void aggiungiTag(@NotNull Taggable taggableObject, @NotNull String tag);

    /**
     * Rimuovi un tag dall'oggetto
     *
     * @param taggableObject l'oggetto da cui rimuovere il tag
     * @param tag            il tag da rimuovere
     */
    void rimuoviTag(@NotNull Taggable taggableObject, @NotNull String tag);

    /**
     * Controlla se l'oggetto ha un tag
     *
     * @param taggableObject l'oggetto su cui eseguire il controllo
     * @param tag            il tag da controllare
     */
    boolean haveTag(@NotNull Taggable taggableObject, @NotNull String tag);
}
