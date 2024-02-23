package ids.unicam.Service;

import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Optional;

public interface CuratoreService {
    PuntoInteresse valutaPuntoInteresse(String usernameCuratore, @NotNull Integer idPuntoInteresse, Boolean stato);

    MaterialeGenerico valutaMateriale(String usernameCuratore, Integer idMaterialeGenerico, Boolean stato);

    void eliminaPuntoInteresse(String usernameCuratore,Integer idPuntoInteresse);

    void eliminaItinerario(String usernameCuratore,Integer idItinerario);

    void eliminaContest(String usernameCuratore,Integer idContest);

    void condividi(String usernameCuratore, Integer idPunto);

    @Transactional
    void elimina(Curatore curatore, MaterialeGenerico materialeGenerico);


    List<Curatore> getAll();

    void deleteById(String username);

    Optional<Curatore> getById(String username);

    void aggiungiOsservatore(String usernameCuratore, String usernameContributorOsservatore) ;

    void rimuoviOsservatore(String usernameCuratore, String usernameContributorOsservatore) ;

    List<Notifica> getNotifiche(String usernameCuratore);

}
