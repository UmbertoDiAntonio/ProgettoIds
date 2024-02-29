package ids.unicam.Service;

import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Optional;

public interface CuratoreService {
    PuntoInteresse valutaPuntoInteresse(String usernameCuratore, @NotNull int idPuntoInteresse, Boolean stato) throws IllegalArgumentException, UnsupportedOperationException, FuoriComuneException;

    MaterialeGenerico valutaMateriale(String usernameCuratore, int idMaterialeGenerico, Boolean stato) throws IllegalArgumentException, UnsupportedOperationException, FuoriComuneException;

    void eliminaPuntoInteresse(String usernameCuratore, int idPuntoInteresse) throws IllegalArgumentException, FuoriComuneException;

    void eliminaItinerario(String usernameCuratore, int idItinerario) throws IllegalArgumentException, FuoriComuneException;

    void eliminaContest(String usernameCuratore, int idContest) throws IllegalArgumentException, FuoriComuneException;

    void eliminaMateriale(String usernameCuratore, int idMateriale) throws IllegalArgumentException, FuoriComuneException;

    Curatore save(Curatore curatore);

    List<Curatore> findByNomeComune(String nomeComune);

    List<Curatore> getAll();

    void deleteByUsername(String username);

    Optional<Curatore> getByUsername(String username);

    List<Notifica> getNotifiche(String usernameCuratore) throws IllegalArgumentException;

}
