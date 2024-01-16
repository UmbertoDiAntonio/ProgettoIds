package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;

import java.time.LocalDate;

public class MuseoFactory implements  PoiFactory {


    @Override
    public PuntoInteresse creaPoi(String nome, Punto punto, LocalDate data) {
        return new Museo(nome,punto,data);
    }

    @Override
    public PuntoInteresse creaPoi(String nome, Punto punto) {
        return new Museo(nome,punto);
    }
}
