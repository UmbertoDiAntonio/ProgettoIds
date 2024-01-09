package ids.unicam.controller;

import ids.unicam.models.Contenuto;
import ids.unicam.models.Itinerario;
import ids.unicam.models.Materiale;
import ids.unicam.models.PuntoInteresse;

import java.util.ArrayList;
import java.util.List;

public class ContenutoController {
    public void deleteContenuto(Contenuto contenuto){
        //TODO
    }

    private List<PuntoInteresse> waitingPoints =new ArrayList<>();
    private List<Itinerario> waitingItinerario=new ArrayList<>();
    private List<Materiale> waitingMaterials =new ArrayList<>();

    public List<Itinerario> getWaitingItinerario() {
        return waitingItinerario;
    }

    public List<Materiale> getWaitingMaterials() {
        return waitingMaterials;
    }

    public List<PuntoInteresse> getWaitingPoints() {
        return waitingPoints;
    }

    public void addPunto(PuntoInteresse puntoInteresse) {
        //TOOD
    }

    public void addMaterialeTo(PuntoInteresse puntoInteresse, Materiale materiale) {
        //TODO
    }

    public void creaItinerario(PuntoInteresse puntoInteresseIniziale) {
        //TODo
    }

    public void addTappa(Itinerario itinerario, PuntoInteresse puntoInteresse) {
        itinerario.addTappa(puntoInteresse);
    }
}
