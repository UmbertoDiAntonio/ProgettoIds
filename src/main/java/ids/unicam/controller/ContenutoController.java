package ids.unicam.controller;

import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ContenutoController {
    private static long id = 0;
    public static long generateID() {
        id+=1;
        return id;
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
    public void addPunto(PuntoInteresse puntoInteresse) {
        contenuti.add(puntoInteresse);
    }

    /**
     * Aggiunge un materiale a un punto di interesse
     * @param puntoInteresse il punto di interesse in cui aggiungere il materiale
     * @param materiale il materiale
     */
    public void addMaterialeTo(PuntoInteresse puntoInteresse, Materiale materiale) {
        puntoInteresse.getMaterialeList().add(materiale);
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
    public void addTappa(Itinerario itinerario, PuntoInteresse puntoInteresse) {
        itinerario.getPercorso().add(puntoInteresse);
    }
    public void addTappa(Itinerario itinerario, PuntoInteresse... puntoInteresse){
        itinerario.getPercorso().addAll(Arrays.stream(puntoInteresse).toList());
    }
    public void removeTappa(Itinerario itinerario, PuntoInteresse puntoInteresse){
        itinerario.getPercorso().remove(puntoInteresse);
    }

    public void deleteContenuto(Contenuto contenuto) {
        if (contenuto instanceof PuntoInteresse) {
            contenuti.remove(contenuto);
        } else if (contenuto instanceof Itinerario itinerario) {
            itinerari.remove(itinerario);
        }
    }
}
