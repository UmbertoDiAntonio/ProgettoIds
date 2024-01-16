package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;

import java.time.LocalDate;
import java.util.Optional;

public class AttivitaFactory implements  PoiFactory{

    @Override
    public PuntoInteresse creaPoi(String nome, Punto punto, LocalDate orario) {
        return new Attivita(nome,punto, orario);
        
    }

    @Override
    public PuntoInteresse creaPoi(String nome, Punto punto) {
        return new Attivita(nome,punto);
    }
}
