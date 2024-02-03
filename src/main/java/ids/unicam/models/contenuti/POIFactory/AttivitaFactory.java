package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.models.Orario;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;


public class AttivitaFactory implements  PoiFactory{


    /**
     *
     * @param nome il nome del punto da creare
     * @param punto la posizione del punto da creare
     * @param orario informazioni sull'orario di attività
     * @return una nuova Attività
     */
    @Override
    public PuntoInteresse creaPoi(String nome, Punto punto, Orario orario) {
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
