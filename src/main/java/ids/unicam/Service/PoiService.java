package ids.unicam.Service;

import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Punto;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.*;
import jakarta.transaction.Transactional;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public interface PoiService {
    PuntoInteresse creaPuntoInteresse(PuntoInteresse puntoInteresse) throws FuoriComuneException;
    PuntoInteresse creaPuntoInteresse(String nomePOI, Punto punto, String usernameCreatore, Tag tag,  TipologiaPuntoInteresse tipologiaPuntoInteresse) throws FuoriComuneException;

    void eliminaPuntoInteresse(int idPuntoInteresse) ;

    void aggiungiMateriale(String usernameTurista, int idPuntoInteresse, MaterialeGenerico materialeGenerico) throws FuoriComuneException;

    List<PuntoInteresse> findActive() ;

    void aggiungiTag(int idPuntoInteresse, Tag tag) ;

    List<Taggable> findByTag(String tag);

    List<Tag> getTags(PuntoInteresse puntoInteresse);

    Optional<PuntoInteresse> getById(int id);

    void deleteById(int id);

    void modificaScadenza(String usernameContributor,int idPuntoInteresse, LocalDate expireDate) throws IllegalArgumentException;

    Stato getStato(int idPuntoInteresse);

    Set<MaterialeGenerico> getMaterialiPoi(int idPunto) throws IllegalArgumentException;

    List<String> getAsList();

    @Transactional
    List<String> getAsListDetailed();

    void setOrario(int idPunto, Orario.OrarioApertura orario, DayOfWeek day);

    List<String> getAsList(List<PuntoInteresse> preferiti);

}
