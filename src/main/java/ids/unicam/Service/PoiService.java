package ids.unicam.Service;

import ids.unicam.models.Punto;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.Orario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import ids.unicam.models.contenuti.puntiInteresse.TipologiaPuntoInteresse;
import jakarta.transaction.Transactional;

import java.util.List;

public interface PoiService {
    PuntoInteresse creaPuntoInteresse(String nomePoi, Punto centroComune, Orario orario, TipologiaPuntoInteresse tipo, Contributor creatore);


    @Transactional
    PuntoInteresse creaPuntoInteresse(String nomePoi, Punto centroComune, TipologiaPuntoInteresse tipo, Contributor creatore);

    void eliminaPuntoInteresse(int idPuntoInteresse) ;

    void aggiungiMateriale(TuristaAutenticato turistaAutenticato, PuntoInteresse puntoInteresse, MaterialeGenerico materialeGenerico) ;

    List<PuntoInteresse> findActive() ;

    void aggiungiTag(PuntoInteresse puntoInteresse, Tag tag) ;

    List<Taggable> findByTag(Tag tag);

    List<Tag> getTags(PuntoInteresse puntoInteresse);
}
