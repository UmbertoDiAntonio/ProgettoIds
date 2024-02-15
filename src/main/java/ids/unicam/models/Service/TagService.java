package ids.unicam.models.Service;

import ids.unicam.models.Repository.TagRepository;
import ids.unicam.models.contenuti.PuntoInteresse;
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
    public void aggiungiTag(PuntoInteresse puntoInteresse, Tag tag) {
        puntoInteresse.getTags().add(tag);
        save(tag);
    }
}
