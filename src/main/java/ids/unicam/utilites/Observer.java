package ids.unicam.utilites;

import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;


public interface Observer {
    void riceviNotifica(Stato eventType, PuntoInteresse puntoInteresse);
    void riceviNotifica(Stato eventType, Materiale materiale);

}
