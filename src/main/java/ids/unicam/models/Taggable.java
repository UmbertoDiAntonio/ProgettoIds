package ids.unicam.models;

import ids.unicam.models.contenuti.Tag;

import java.util.Set;

public interface Taggable {
    Set<Tag> getTags();
    void addTag(Tag tag);
}
