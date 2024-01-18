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
    private final HashMap<PuntoInteresse, Set<Materiale>> materialiPerPunto = new HashMap<>();

    private final HashMap<String, Itinerario> itinerari = new HashMap<>();

    public List<PuntoInteresse> getContenuti() {
        return contenuti;
    }

    public HashMap<String, Itinerario> getItinerari() {
        return itinerari;
    }

    public void addPunto(PuntoInteresse puntoInteresse) {
        contenuti.add(puntoInteresse);
    }

    public void addMaterialeTo(PuntoInteresse puntoInteresse, Materiale materiale) {
        materiale.setApproved(false);
        materialiPerPunto
                .computeIfAbsent(puntoInteresse, k -> new HashSet<>())
                .add(materiale);
    }

    public Itinerario creaItinerario(String nome,PuntoInteresse... puntoInteresse) {
        Itinerario itinerario = new Itinerario(nome, puntoInteresse);
        itinerari.put(nome, itinerario);

        return itinerario;
    }

    public void addTappa(Itinerario itinerario, PuntoInteresse puntoInteresse) {
        itinerario.addTappa(puntoInteresse);
    }

    public void deleteContenuto(Contenuto contenuto) {
        if (contenuto instanceof PuntoInteresse) {
            contenuti.remove(contenuto);
            materialiPerPunto.remove(contenuto);
        } else if (contenuto instanceof Itinerario) {
            itinerari.remove(((Itinerario) contenuto).getNome());

        }
    }
}
