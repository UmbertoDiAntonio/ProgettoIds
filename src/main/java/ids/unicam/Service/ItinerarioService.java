package ids.unicam.Service;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

public interface ItinerarioService {

    boolean aggiungiTappa(Itinerario itinerario, PuntoInteresse puntoInteresse);

    void aggiungiTappa(Itinerario itinerario, PuntoInteresse... puntiInteresse);

    void rimuoviTappa(Itinerario itinerario, PuntoInteresse puntoInteresse);

    Itinerario creaItinerario(Comune comune, String nome, PuntoInteresse... puntiInteresse);
}
