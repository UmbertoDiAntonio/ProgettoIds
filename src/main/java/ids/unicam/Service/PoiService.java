package ids.unicam.Service;

import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import jakarta.transaction.Transactional;

import java.util.List;

public interface PoiService {
    @Transactional
    void eliminaPuntoInteresse(int idPuntoInteresse) ;

    void aggiungiMateriale(TuristaAutenticato turistaAutenticato, PuntoInteresse puntoInteresse, MaterialeGenerico materialeGenerico) ;

    @Transactional
    List<PuntoInteresse> findActive() ;

    @Transactional
    void aggiungiTag(PuntoInteresse puntoInteresse, Tag tag) ;

    List<Taggable> findByTag(Tag tag);

    List<Tag> getTags(PuntoInteresse puntoInteresse);
}
