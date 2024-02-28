package ids.unicam.Service;

import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Optional;

public interface CuratoreService {
    PuntoInteresse valutaPuntoInteresse(String usernameCuratore, @NotNull Integer idPuntoInteresse, Boolean stato) throws IllegalArgumentException, UnsupportedOperationException, FuoriComuneException;

    MaterialeGenerico valutaMateriale(String usernameCuratore, Integer idMaterialeGenerico, Boolean stato) throws IllegalArgumentException, UnsupportedOperationException, FuoriComuneException;

    void eliminaPuntoInteresse(String usernameCuratore,Integer idPuntoInteresse) throws IllegalArgumentException, FuoriComuneException;

    void eliminaItinerario(String usernameCuratore,Integer idItinerario) throws IllegalArgumentException, FuoriComuneException;

    void eliminaContest(String usernameCuratore,Integer idContest) throws IllegalArgumentException, FuoriComuneException;

    void condividi(String usernameCuratore, Integer idPunto)throws IllegalArgumentException;

    @Transactional
    void elimina(Curatore curatore, MaterialeGenerico materialeGenerico) throws FuoriComuneException;


    List<Curatore> getAll();

    void deleteById(String username);

    Optional<Curatore> getById(String username);

    List<Notifica> getNotifiche(String usernameCuratore) throws IllegalArgumentException;

}
