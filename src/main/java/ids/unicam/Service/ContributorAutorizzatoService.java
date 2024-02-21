package ids.unicam.Service;

import ids.unicam.models.Comune;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

import java.time.LocalDate;

public interface ContributorAutorizzatoService {

     Itinerario aggiungiItinerario(Comune comune, String nome, PuntoInteresse... puntiInteresse);

     boolean aggiungiTappaItinerario(Itinerario itinerario,PuntoInteresse puntoInteresse);

     void aggiungiTappaItinerario(Itinerario itinerario,PuntoInteresse... puntiInteresse);

     void modificaScadenza(PuntoInteresse puntoInteresse, LocalDate expireDate);
}
