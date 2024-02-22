package ids.unicam.Service;

import ids.unicam.models.DTO.RichiestaCreazioneTagDTO;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.Tag;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface PoiService {
    PuntoInteresse creaPuntoInteresse(PuntoInteresse puntoInteresse);

    void eliminaPuntoInteresse(int idPuntoInteresse) ;

    void aggiungiMateriale(TuristaAutenticato turistaAutenticato, PuntoInteresse puntoInteresse, MaterialeGenerico materialeGenerico) ;

    List<PuntoInteresse> findActive() ;

    void aggiungiTag(PuntoInteresse puntoInteresse, Tag tag) ;

    List<Taggable> findByTag(RichiestaCreazioneTagDTO tagDTO);

    List<Tag> getTags(PuntoInteresse puntoInteresse);

    Optional<PuntoInteresse> getById(int id);

    PuntoInteresse update(PuntoInteresse puntoInteresse, int id);

    void deleteById(int id);

    void modificaScadenza(String usernameContributor,Integer idPuntoInteresse, LocalDate expireDate) ;

    Stato getStato(int idPuntoInteresse);
}
