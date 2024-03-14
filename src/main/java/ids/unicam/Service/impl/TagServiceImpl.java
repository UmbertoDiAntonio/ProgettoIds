package ids.unicam.Service.impl;

import ids.unicam.Service.TagService;
import ids.unicam.models.contenuti.Taggable;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;

@Service
public class TagServiceImpl implements TagService {

    @Transactional
    @Override
    public void aggiungiTag(@NotNull Taggable taggableObject,@NotNull  String tag) {
        taggableObject.addTag(tag);
    }

    @Transactional
    @Override
    public void rimuoviTag(@NotNull Taggable taggableObject, @NotNull String tag) {
        taggableObject.rimuoviTag(tag);
    }

    @Override
    public boolean haveTag(@NotNull Taggable taggableObject,@NotNull  String tag) {
        return taggableObject.getTags().contains(tag);
    }
}
