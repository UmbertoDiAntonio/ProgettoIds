package ids.unicam.controller;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.*;


import java.util.*;

public class ContenutoController {
    private static long id = 0;

    public static long generateID() {
        id+=1;
        return id;
    }
    private final List<Materiale> waitingMaterials =new ArrayList<>();
    private final List<Contenuto> waiting = new ArrayList<>();
    private final HashMap<PuntoInteresse, Set<Materiale>> materialiPerPunto = new HashMap<>();
    private final HashMap<String, Itinerario> itinerari = new HashMap<>();

    public List<Contenuto> getWaiting() {
        return waiting;
    }

    public List<Materiale> getWaitingMaterials() {
        return waitingMaterials;
    }

    public void addPunto(PuntoInteresse puntoInteresse) {
        waiting.add(puntoInteresse);

    }

    public void addMaterialeTo(PuntoInteresse puntoInteresse, Materiale materiale) {
        materiale.setApproved(false);
        materialiPerPunto
                .computeIfAbsent(puntoInteresse, k -> new HashSet<>())
                .add(materiale);
    }

    public void creaItinerario(String nome,PuntoInteresse... puntoInteresse) {
        Itinerario itinerario = new Itinerario(nome, puntoInteresse);
        itinerari.put(nome, itinerario);
        itinerario.addTappa(puntoInteresse);
    }

    public void addTappa(Itinerario itinerario, PuntoInteresse puntoInteresse) {
        itinerario.addTappa(puntoInteresse);
    }

    public void deleteContenuto(Contenuto contenuto) {
        if (contenuto instanceof PuntoInteresse) {
            waiting.remove(contenuto);
            materialiPerPunto.remove(contenuto);
        } else if (contenuto instanceof Itinerario) {
            itinerari.remove(((Itinerario) contenuto).getNome());

        }
    }
}
