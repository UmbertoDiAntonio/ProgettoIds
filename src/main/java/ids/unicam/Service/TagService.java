package ids.unicam.Service;

import ids.unicam.models.contenuti.Taggable;

public interface TagService {

    void aggiungiTag(Taggable taggableObject, String tag);

    void rimuoviTag(Taggable taggableObject, String tag);

    boolean haveTag(Taggable taggableObject, String tag);
}
