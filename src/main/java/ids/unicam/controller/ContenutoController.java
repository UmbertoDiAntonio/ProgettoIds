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
    public void deleteContenuto(Contenuto contenuto) {
        //TODO
    }

    Comune comune;

    public ContenutoController(Comune comune) {
        this.comune = comune;
    }

    private List<PuntoInteresse> waitingPoints = new ArrayList<>();
    private List<Itinerario> waitingItinerario = new ArrayList<>();
    private List<Materiale> waitingMaterials = new ArrayList<>();

    public List<Itinerario> getWaitingItinerario() {
        return waitingItinerario;
    }

    public List<Materiale> getWaitingMaterials() {
        return waitingMaterials;
    }

    public List<PuntoInteresse> getWaitingPoints() {
        return waitingPoints;
    }

    public void addPunto(PuntoInteresse puntoInteresse, boolean approved) {
        if (approved)
            comune.getContenuti().add(puntoInteresse);
        else
            comune.getContenutoController().waitingPoints.add(puntoInteresse);

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
