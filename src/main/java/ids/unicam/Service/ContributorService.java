package ids.unicam.Service;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

import java.time.LocalDate;

public interface ContributorService {
    Itinerario aggiungiItinerario(Comune comune, String nome, PuntoInteresse... puntoInteresse) ;

    boolean aggiungiTappaItinerario(Itinerario itinerario, PuntoInteresse puntoInteresse);
    void modificaScadenza(PuntoInteresse puntoInteresse, LocalDate expireDate);
}
