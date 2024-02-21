package ids.unicam.Service;

import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

import java.time.LocalDate;

public interface ContributorAutorizzatoService {

     Itinerario aggiungiItinerario(Itinerario itinerario);

     boolean aggiungiTappaItinerario(Itinerario itinerario,PuntoInteresse puntoInteresse);

     void aggiungiTappaItinerario(Itinerario itinerario,PuntoInteresse... puntiInteresse);

     void modificaScadenza(PuntoInteresse puntoInteresse, LocalDate expireDate);
}
