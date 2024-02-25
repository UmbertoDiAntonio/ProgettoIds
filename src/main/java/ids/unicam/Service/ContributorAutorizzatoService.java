package ids.unicam.Service;

import ids.unicam.models.attori.ContributorAutorizzato;

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

}
