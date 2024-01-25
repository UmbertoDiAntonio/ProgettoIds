package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;

import java.time.LocalDate;

public class AttivitaFactory implements  PoiFactory{

    private final LocalDate data;

    public AttivitaFactory(LocalDate data) {
        this.data = data;
    }

    @Override
    public PuntoInteresse creaPoi(String nome, Punto punto, LocalDate orario) {
        return new Attivita(nome,punto, orario);
        
    }

    @Override
    public PuntoInteresse creaPoi(String nome, Punto punto) {
        return new Attivita(nome,punto);
    }
}
