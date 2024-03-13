package ids.unicam.Service.impl;

import ids.unicam.Service.TagService;
import ids.unicam.models.contenuti.Taggable;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

@Service
public class TagServiceImpl implements TagService {


    @Transactional
    @Override
    public void aggiungiTag(Taggable taggableObject, String tag) {
        taggableObject.addTag(tag);
    }

    @Transactional
    @Override
    public void rimuoviTag(Taggable taggableObject, String tag) {
        taggableObject.rimuoviTag(tag);
    }

    @Override
    public boolean haveTag(Taggable taggableObject, String tag) {
        return taggableObject.getTags().contains(tag);
    }
}
