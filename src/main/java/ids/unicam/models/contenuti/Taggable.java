package ids.unicam.models.contenuti;

import ids.unicam.models.contenuti.puntiInteresse.Tag;

import java.util.Set;

public interface Taggable {
    Set<Tag> getTags();
    void addTag(Tag tag);
}
