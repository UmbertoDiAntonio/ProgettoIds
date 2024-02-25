package ids.unicam.Service;

import ids.unicam.models.DTO.PuntoInteresseDTO;
import ids.unicam.models.contenuti.Taggable;

import java.util.List;

public interface TuristaService {
    List<Taggable> findByTag(String tag);

    void report(PuntoInteresseDTO poiDTO, String msg);

}
