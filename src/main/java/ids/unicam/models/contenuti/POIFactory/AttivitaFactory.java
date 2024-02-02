package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;

import java.time.LocalDate;


public class AttivitaFactory implements  PoiFactory{

    private final LocalDate data;

    public AttivitaFactory(LocalDate data) {
        this.data = data;
    }

    /**
     *
     * @param nome il nome del punto da creare
     * @param punto la posizione del punto da creare
     * @param orario informazioni sull'orario di attività
     * @return una nuova Attività
     */
    @Override
    public PuntoInteresse creaPoi(String nome, Punto punto, LocalDate orario) {
        return new Attivita(nome,punto, orario);
        
    }

    /**
     *
     * @param nome il nome del punto da creare
     * @param punto la posizione del punto da creare
     * @return ritorna una nuova Attività
     */
    @Override
    public PuntoInteresse creaPoi(String nome, Punto punto) {
        return new Attivita(nome,punto);
    }
}
