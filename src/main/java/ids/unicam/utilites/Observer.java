package ids.unicam.utilites;

import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;


public interface Observer {
    void riceviNotifica(boolean eventType, PuntoInteresse puntoInteresse);
    void riceviNotifica(boolean eventType, Materiale materiale);

}
