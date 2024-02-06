package ids.unicam.controller;

import ids.unicam.models.contenuti.ContenutoGenerico;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.models.contenuti.PuntoInteresse;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


public class ContenutoController {
    private static long _id = 0;


    public static long generaID() {
        _id+=1;
        return _id;
    }

    private final List<PuntoInteresse> contenuti = new ArrayList<>();


    private final ArrayList<Itinerario> itinerari = new ArrayList<>();

    public List<PuntoInteresse> getContenuti() {
        return contenuti;
    }

    public ArrayList<Itinerario> getItinerari() {
        return itinerari;
    }

    /**
     * Aggiunge il punto di interesse alla lista dei contenuti
     * @param puntoInteresse il Punto di interesse da aggiungere
     */
    public void aggiungiPuntoInteresse(PuntoInteresse puntoInteresse) {
        contenuti.add(puntoInteresse);
    }

    /**
     * Aggiunge un materiale a un punto di interesse
     * @param puntoInteresse il punto di interesse in cui aggiungere il materiale
     * @param materialeGenerico il materiale
     */
    public void aggiungiMateriale(PuntoInteresse puntoInteresse, MaterialeGenerico materialeGenerico) {
        puntoInteresse.getMateriali().add(materialeGenerico);
    }

    /**
     * Crea e aggiunge un itinerario alla lista di itinerari
     * @param nome nome da dare all'itinerario
     * @param puntoInteresse punti di interesse che ne fanno parte
     * @return l'itinerario appena creato
     */
    public Itinerario creaItinerario(String nome,PuntoInteresse... puntoInteresse) {
        Itinerario itinerario = new Itinerario(nome, puntoInteresse);
        itinerari.add(itinerario);

        return itinerario;
    }

    /**
     * Aggiunge una nuova tappa a un itinerario esistente
     * @param itinerario l'itinerario a cui aggiungere la tappa
     * @param puntoInteresse il punto di interesse da aggiungere come tappa
     */
    public void aggiungiTappa(Itinerario itinerario, PuntoInteresse puntoInteresse) {
        itinerario.getPercorso().add(puntoInteresse);
    }
    public void aggiungiTappa(Itinerario itinerario, PuntoInteresse... puntoInteresse){
        itinerario.getPercorso().addAll(Arrays.stream(puntoInteresse).toList());
    }
    public void rimuoviTappa(Itinerario itinerario, PuntoInteresse puntoInteresse){
        itinerario.getPercorso().remove(puntoInteresse);
    }

    public void eliminaContenuto(ContenutoGenerico contenutoGenerico) {
        if (contenutoGenerico instanceof PuntoInteresse) {
            contenuti.remove(contenutoGenerico);
        } else if (contenutoGenerico instanceof Itinerario itinerario) {
            itinerari.remove(itinerario);
        }
    }


}
