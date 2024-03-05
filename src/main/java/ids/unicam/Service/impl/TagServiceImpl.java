package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.TagRepository;
import ids.unicam.Service.TagService;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

@Service
public class TagServiceImpl implements TagService {
    private final TagRepository repository;

    public TagServiceImpl(TagRepository repository) {
        this.repository = repository;
    }

    @Transactional
    @Override
    public Tag save(Tag tag) {
        return repository.save(tag);
    }

    @Transactional
    @Override
    public void aggiungiTag(Taggable taggableObject, Tag tag) {
        taggableObject.addTag(tag);
        save(tag);
    }

    @Transactional
    @Override
    public void rimuoviTag(Taggable taggableObject, Tag tag) {
        taggableObject.rimuoviTag(tag);
        save(tag);
    }

    @Override
    public boolean haveTag(Taggable taggableObject, Tag tag) {
        return taggableObject.getTags().contains(tag);
    }
}
