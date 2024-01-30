package ids.unicam.models.attori;

import ids.unicam.Comune;
import ids.unicam.models.contenuti.*;
import ids.unicam.utilites.Observer;
import ids.unicam.utilites.Punto;

import java.util.ArrayList;

public class Curatore extends ContributorTrusted {

    private final ArrayList<Observer> osservatori = new ArrayList<>();

    public void aggiungiOsservatore(Observer osservatore) {
        osservatori.add(osservatore);
    }

    public void rimuoviOsservatore(Observer osservatore) {
        osservatori.remove(osservatore);
    }

    public void notifica(Boolean eventType, PuntoInteresse puntoInteresse) {
        for (Observer listener : osservatori) {
            listener.riceviNotifica(eventType, puntoInteresse);
        }
    }

    public ArrayList<Observer> getOsservatori() {
        return osservatori;
    }

    public void notifica(Boolean eventType, Materiale materiale) {

        for (Observer listener : osservatori) {
            listener.riceviNotifica(eventType, materiale);
        }
    }

    protected Curatore(Comune comune, Contributor contributor) {
        super(comune, contributor);
    }

    public void share(Contenuto contenuto) {
        //TODO
    }

    /**
     * Valuta un Materiale, in caso di non approvazione lo rimuove dalla lista di materiali in attesa di approvazione del contest???????,
     * notifica i subscriber
     *
     * @param materiale il materiale che si vuole valutare
     * @param approvato approvato o non approvato
     */
    public void valuta(Materiale materiale, boolean approvato) {
        materiale.setApproved(approvato);
        notifica(approvato, materiale);
    }

    /**
     * Valuta un punto di interesse, in caso di non approvazione lo rimuove dalla lista dei contenuti nel controller del comune associato,
     * notifica i subscriber
     *
     * @param puntoInteresse il punto di interesse che si vuole valutare
     * @param approvato      stato punto di interesse : approvato/non approvato
     */
    public void valuta(PuntoInteresse puntoInteresse, boolean approvato) {
        puntoInteresse.setApproved(approvato);
        if (!approvato)
            getComune().getContenutoController().getContenuti().remove(puntoInteresse);

        notifica(approvato, puntoInteresse);
    }


    public void delete(Contenuto contenuto) {
        if (contenuto instanceof Contest contest) {
            getComune().getContestController().deleteContest(contest);
        } else {
            getComune().getContenutoController().deleteContenuto(contenuto);
        }
    }

    public void rimuoviTappa(Itinerario itinerario, PuntoInteresse puntoInteresse){
        getComune().getContenutoController().removeTappa(itinerario, puntoInteresse);
    }

}
