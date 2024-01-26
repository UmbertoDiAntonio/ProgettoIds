package ids.unicam.models.attori;

import ids.unicam.Comune;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Observer;

import java.util.ArrayList;

public class Curatore extends ContributorTrusted {

    private final ArrayList<Observer> osservatori = new ArrayList<>();

    public void aggiungiOsservatore(Observer osservatore) {
        osservatori.add(osservatore);
    }

    //TODo test
    public void rimuoviOsservatore(Observer osservatore) {
        osservatori.remove(osservatore);
    }

    public void notifica(Boolean eventType, PuntoInteresse puntoInteresse) {
        for (Observer listener : osservatori) {
            listener.riceviNotifica(eventType, puntoInteresse);
        }
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
     * Approva o non approva un Materiale, in caso di non approvazione lo rimuove dalla lista di materiali in attesa di approvazione del contest???????,
     * notifica i subscriber
     *
     * @param materiale il materiale che si vuole valutare
     * @param approvato approvato o non approvato
     */
    public void approva(Materiale materiale, boolean approvato) { //TODO rename
        materiale.setApproved(approvato);
        notifica(approvato, materiale);
    }

    /**
     * Approva o non approva un punto di interesse, in caso di non approvazione lo rimuove dalla lista dei contenuti nel controller del comune associato,
     * notifica i subscriber
     *
     * @param puntoInteresse il punto di interesse che si vuole valutare
     * @param approvato approvato o non approvato
     */
    public void approva(PuntoInteresse puntoInteresse, boolean approvato) { //TODO rename
        puntoInteresse.setApproved(approvato);
        if (!approvato)
            getComune().getContenutoController().getContenuti().remove(puntoInteresse);

        notifica(approvato, puntoInteresse);
    }


    //TODO test
    public void delete(Contenuto contenuto) {
        getComune().getContenutoController().deleteContenuto(contenuto);
    }

}
