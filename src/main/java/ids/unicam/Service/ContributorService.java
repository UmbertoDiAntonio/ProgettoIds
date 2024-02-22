package ids.unicam.Service;

import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface ContributorService {
    //Itinerario aggiungiItinerario(Itinerario itinerario) ;

   // boolean aggiungiTappaItinerario(Itinerario itinerario, PuntoInteresse puntoInteresse);

   // void modificaScadenza(PuntoInteresse puntoInteresse, LocalDate expireDate);

    List<Contributor> getAll();

    void deleteById(String username);

    Optional<Contributor> getById(String username);

    Contributor update(RichiestaCreazioneContributorDTO contributorDTO, String username);


}
