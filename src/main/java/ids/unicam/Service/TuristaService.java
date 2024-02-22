package ids.unicam.Service;

import ids.unicam.models.DTO.RichiestaCreazionePoiDTO;
import ids.unicam.models.DTO.RichiestaCreazioneTagDTO;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.Tag;

import java.util.List;
import java.util.Optional;

public interface TuristaService {
    List<Taggable> findByTag(RichiestaCreazioneTagDTO tagDTO);

    void report(RichiestaCreazionePoiDTO
                        poiDTO, String msg);

    Optional<TuristaAutenticato> accedi(String username, String password);
}
