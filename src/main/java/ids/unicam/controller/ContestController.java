package ids.unicam.controller;

import ids.unicam.models.Contest;
import ids.unicam.models.Materiale;
import ids.unicam.models.attori.TuristaLoggato;

import java.util.ArrayList;
import java.util.List;

public class ContestController {
    private List<Materiale> waitingMaterials =new ArrayList<>();

    public List<Materiale> getWaitingMaterials() {
        return waitingMaterials;
    }
    public void invita(Contest contest, TuristaLoggato turistaLoggato){

    }
}
