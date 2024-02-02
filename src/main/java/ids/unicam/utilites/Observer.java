package ids.unicam.utilites;

import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.models.contenuti.Status;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;


public interface Observer {
    void riceviNotifica(Status eventType, PuntoInteresse puntoInteresse);
    void riceviNotifica(Status eventType, Materiale materiale);

}
