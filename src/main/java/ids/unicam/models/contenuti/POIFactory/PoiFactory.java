package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;

import java.time.LocalDate;
import java.util.Optional;

public interface PoiFactory {
    PuntoInteresse creaPoi(String nome, Punto punto, LocalDate data);
    PuntoInteresse creaPoi(String nome, Punto punto);
}

