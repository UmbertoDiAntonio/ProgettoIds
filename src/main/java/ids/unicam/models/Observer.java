package ids.unicam.models;

import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;


public interface Observer {
    void riceviNotifica(Stato eventType, PuntoInteresse puntoInteresse);
    void riceviNotifica(Stato eventType, MaterialeGenerico materialeGenerico);

}
