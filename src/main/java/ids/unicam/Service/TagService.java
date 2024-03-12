package ids.unicam.Service;

import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.puntiInteresse.Tag;

public interface TagService {

    Tag save(Tag tag);

    void aggiungiTag(Taggable taggableObject, Tag tag);

    void rimuoviTag(Taggable taggableObject, Tag tag);

    boolean haveTag(Taggable taggableObject, Tag tag);
}
