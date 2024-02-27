package ids.unicam.Service;

import ids.unicam.models.contenuti.Taggable;

import java.util.List;

public interface TuristaService {
    List<Taggable> findByTag(String tag);

    void report(int idPuntoInteresse, String msg);

}
