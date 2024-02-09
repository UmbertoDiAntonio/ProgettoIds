package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.Comune;
import ids.unicam.models.Orario;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;

public interface PoiFactory {
    /**
     * Crea un punto di interesse di tipo attività
     * @param nome il nome del punto da creare
     * @param punto la posizione del punto da creare
     * @param data informazioni sull'orario di attività
     * @return il punto di interesse creato
     */
    PuntoInteresse creaPoi(Comune comune,String nome, Punto punto, Orario data);
    /**
     * Crea un punto di interesse di tipo attività
     * @param nome il nome del punto da creare
     * @param punto la posizione del punto da creare
     * @return il punto di interesse creato
     */
    PuntoInteresse creaPoi(Comune comune, String nome, Punto punto);
}

