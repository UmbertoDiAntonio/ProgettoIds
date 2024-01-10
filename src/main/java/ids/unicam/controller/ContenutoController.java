package ids.unicam.controller;

import ids.unicam.models.Comune;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;

import java.util.ArrayList;
import java.util.List;

public class ContenutoController {
    private static ContenutoController instance = null;

    public static ContenutoController getInstance() {
        if(instance==null)
            instance=new ContenutoController();
        return instance;
    }

    private ContenutoController() {    }

    private static long id = 0;

    public static long generateID() {
        id+=1;
        return id;
    }

    public void deleteContenuto(Contenuto contenuto) {
        //TODO
    }

    private final List<Contenuto> contenuti = new ArrayList<>();

    public List<Contenuto> getContenuti() {
        return contenuti;
    }



    private final List<Contenuto> waitingContents = new ArrayList<>();
    private final List<Materiale> waitingMaterials = new ArrayList<>();

    public List<Contenuto> getWaiting() {
        return waitingContents;
    }

    public List<Materiale> getWaitingMaterials() {
        return waitingMaterials;
    }


    public void addPunto(PuntoInteresse puntoInteresse) {
            contenuti.add(puntoInteresse);
    }

    public void addMaterialeTo(PuntoInteresse puntoInteresse, Materiale materiale) {
        puntoInteresse.addMateriale(materiale);
    }

    public void creaItinerario(PuntoInteresse puntoInteresseIniziale) {
        //TODo
    }

    public void addTappa(Itinerario itinerario, PuntoInteresse puntoInteresse) {
            itinerario.addTappa(puntoInteresse);
    }
    

}
