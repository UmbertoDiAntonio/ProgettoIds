package ids.unicam.Service;

import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.materiali.TipologiaMateriale;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.Tag;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface PoiService {
    PuntoInteresse creaPuntoInteresse(PuntoInteresse puntoInteresse) throws FuoriComuneException;

    void eliminaPuntoInteresse(int idPuntoInteresse) ;

    void aggiungiMateriale(String usernameTurista, Integer idPuntoInteresse, MaterialeGenerico materialeGenerico) throws FuoriComuneException;

    List<PuntoInteresse> findActive() ;

    void aggiungiTag(PuntoInteresse puntoInteresse, Tag tag) ;

    List<Taggable> findByTag(String tag);

    List<Tag> getTags(PuntoInteresse puntoInteresse);

    Optional<PuntoInteresse> getById(int id);

    void deleteById(int id);

    void modificaScadenza(String usernameContributor,Integer idPuntoInteresse, LocalDate expireDate) throws IllegalArgumentException;

    Stato getStato(int idPuntoInteresse);

    List<MaterialeGenerico> getMaterialiPoi(Integer idPunto);
}
