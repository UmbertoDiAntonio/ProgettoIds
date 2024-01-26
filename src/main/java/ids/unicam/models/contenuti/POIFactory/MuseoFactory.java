package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;

import java.time.LocalDate;

public class MuseoFactory implements PoiFactory {


    /**
     *
     * @param nome il nome del punto da creare
     * @param punto la posizione del punto da creare
     * @param data informazioni sull'orario di attività
     * @return
     */
    @Override
    public PuntoInteresse creaPoi(String nome, Punto punto, LocalDate data) {
        return new Museo(nome, punto, data);
    }

    /**
     *
     * @param nome il nome del punto da creare
     * @param punto la posizione del punto da creare
     * @return
     */
    @Override
    public PuntoInteresse creaPoi(String nome, Punto punto) {
        return new Museo(nome, punto);
    }
}
