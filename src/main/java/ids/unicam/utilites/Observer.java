package ids.unicam.utilites;

import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.models.contenuti.PuntoInteresse;


public interface Observer {
    void riceviNotifica(Stato eventType, PuntoInteresse puntoInteresse);
    void riceviNotifica(Stato eventType, MaterialeGenerico materialeGenerico);

}
