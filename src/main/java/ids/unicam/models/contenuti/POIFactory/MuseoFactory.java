package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.models.Orario;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;

public class MuseoFactory implements PoiFactory {


    /**
     *
     * @param nome il nome del punto da creare
     * @param punto la posizione del punto da creare
     * @param data informazioni sull'orario di attività
     * @return un nuovo Museo
     */
    @Override
    public PuntoInteresse creaPoi(String nome, Punto punto, Orario data) {
        return new Museo(nome, punto, data);
    }

    /**
     *
     * @param nome il nome del punto da creare
     * @param punto la posizione del punto da creare
     * @return un nuovo Museo
     */
    @Override
    public PuntoInteresse creaPoi(String nome, Punto punto) {
        return new Museo(nome, punto);
    }
}
