package ids.unicam.Service;

import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface ContributorAutorizzatoService {

    //Itinerario aggiungiItinerario(Itinerario itinerario);

     //boolean aggiungiTappaItinerario(Itinerario itinerario,PuntoInteresse puntoInteresse);

     //void aggiungiTappaItinerario(Itinerario itinerario,PuntoInteresse... puntiInteresse);

     //void modificaScadenza(PuntoInteresse puntoInteresse, LocalDate expireDate);

     List<ContributorAutorizzato> getAll();

     void deleteById(String username);

     Optional<ContributorAutorizzato> getById(String username);

     ContributorAutorizzato update(RichiestaCreazioneContributorDTO contributorDTO, String username);

}
