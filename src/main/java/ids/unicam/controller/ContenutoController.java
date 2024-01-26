package ids.unicam.controller;

import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;

import java.util.*;

public class ContenutoController {
    private static long id = 0;

    public static long generateID() {
        id+=1;
        return id;
    }
    private final List<PuntoInteresse> contenuti = new ArrayList<>();

    private final HashMap<String, Itinerario> itinerari = new HashMap<>();

    public List<PuntoInteresse> getContenuti() {
        return contenuti;
    }

    public HashMap<String, Itinerario> getItinerari() {
        return itinerari;
    }

    /**
     * Aggiunge il punto di interesse alla lista dei contenuti
     * @param puntoInteresse il Punto di interesse da aggiugnere
     */
    public void addPunto(PuntoInteresse puntoInteresse) {
        contenuti.add(puntoInteresse);
    }

    //TODO capire perche materiale approved false
    /**
     * Aggiunge un materiale non approvato? a un punto di interesse
     * @param puntoInteresse il punto di interesse in cui aggiungere il materiale
     * @param materiale il materiale
     */
    public void addMaterialeTo(PuntoInteresse puntoInteresse, Materiale materiale) {
        materiale.setApproved(false);
        puntoInteresse.addMateriale(materiale);
    }

    /**
     * Crea e aggiunge un itinerario alla lista di itinerari
     * @param nome nome da dare all'itinerario
     * @param puntoInteresse punti di interesse che ne fanno parte
     * @return l'itinerario appena creato
     */
    public Itinerario creaItinerario(String nome,PuntoInteresse... puntoInteresse) {
        Itinerario itinerario = new Itinerario(nome, puntoInteresse);
        itinerari.put(nome, itinerario);

        return itinerario;
    }

    /**
     * aggiunge una nuova tappa a un itinerario esistente
     * @param itinerario l'itinerario a cui aggiungere la tappa
     * @param puntoInteresse il punto di interesse da aggiungere come tappa
     */
    public void addTappa(Itinerario itinerario, PuntoInteresse puntoInteresse) {
        itinerario.addTappa(puntoInteresse);
    }

    //TODO testare
    public void deleteContenuto(Contenuto contenuto) {
        if (contenuto instanceof PuntoInteresse) {
            contenuti.remove(contenuto);
        } else if (contenuto instanceof Itinerario) {
            itinerari.remove(((Itinerario) contenuto).getNome());

        }
    }
}
