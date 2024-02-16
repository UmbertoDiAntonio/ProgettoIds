package ids.unicam.Service;

import ids.unicam.DataBase.Repository.TagRepository;
import ids.unicam.models.Taggable;
import ids.unicam.models.contenuti.Tag;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

@Service
public class TagService {
    private final TagRepository repository;

    public TagService(TagRepository repository) {
        this.repository = repository;
    }

    @Transactional
    public Tag save(Tag tag) {
        return repository.save(tag);
    }

    @Transactional
    public void aggiungiTag(Taggable taggableObject, Tag tag) {
        taggableObject.addTag(tag);
        save(tag);
    }

    public boolean haveTag(Taggable taggableObject,Tag tag) {
        return taggableObject.getTags().contains(tag);
    }
}
