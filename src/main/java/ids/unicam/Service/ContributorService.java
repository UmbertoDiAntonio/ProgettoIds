package ids.unicam.Service;

import ids.unicam.models.Comune;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

import java.time.LocalDate;

public interface ContributorService {
    PuntoInteresse aggiungiPuntoInteresse(Contributor contributor, PuntoInteresse puntoInteresse) ;
    Itinerario aggiungiItinerario(Comune comune, String nome, PuntoInteresse... puntoInteresse) ;

    boolean aggiungiTappaItinerario(Itinerario itinerario, PuntoInteresse puntoInteresse);
    void modificaScadenza(PuntoInteresse puntoInteresse, LocalDate expireDate);
}
