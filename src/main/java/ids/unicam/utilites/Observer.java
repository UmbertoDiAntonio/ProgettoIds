package ids.unicam.utilites;

import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;

public interface Observer {
    void riceviNotifica(boolean eventType, PuntoInteresse puntoInteresse);
    void riceviNotifica(boolean eventType, Materiale materiale);

}
