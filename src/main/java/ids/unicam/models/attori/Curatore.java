package ids.unicam.models.attori;

import ids.unicam.Comune;
import ids.unicam.models.contenuti.*;
import ids.unicam.utilites.Observer;
import jakarta.persistence.Entity;
import jakarta.persistence.Transient;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;


@Entity
public class Curatore extends ContributorAutorizzato {

    @Transient
    private final ArrayList<Observer> osservatori = new ArrayList<>();

    public Curatore() {

    }

    public void aggiungiOsservatore(Observer osservatore) {
        osservatori.add(osservatore);
    }

    public void rimuoviOsservatore(Observer osservatore) {
        osservatori.remove(osservatore);
    }

    public void notifica(Status eventType, PuntoInteresse puntoInteresse) {
        for (Observer listener : osservatori) {
            listener.riceviNotifica(eventType, puntoInteresse);
        }
    }

    
    public ArrayList<Observer> getOsservatori() {
        return osservatori;
    }

    public void notifica(Status eventType, Materiale materiale) {

        for (Observer listener : osservatori) {
            listener.riceviNotifica(eventType, materiale);
        }
    }

    protected Curatore(Comune comune, Contributor contributor) {
        super(comune, contributor);
    }

    public void condividi(Contenuto contenuto) {
        throw new UnsupportedOperationException(contenuto.getId()+"non può ancora essere condiviso");
        //TODO
    }

    /**
     * Valuta un Materiale,
     * notifica i subscriber
     *
     * @param materiale il materiale che si vuole valutare
     * @param approvato approvato o non approvato
     */
    public void valuta(Materiale materiale, Status approvato) {
        materiale.setStato(approvato);
        notifica(approvato, materiale);
    }

    /**
     * Valuta un punto di interesse, in caso di non approvazione lo rimuove dalla lista dei contenuti nel controller del comune associato,
     * notifica i subscriber
     *
     * @param puntoInteresse il punto di interesse che si vuole valutare
     * @param approvato      stato punto di interesse: approvato/non approvato
     */
    public void valuta(@NotNull PuntoInteresse puntoInteresse, Status approvato) {
        puntoInteresse.setStato(approvato);
        if (approvato==Status.NOT_APPROVED)
            getComune().getContenutoController().getContenuti().remove(puntoInteresse);

        notifica(approvato, puntoInteresse);
    }


    public void elimina(Contenuto contenuto) {
        if (contenuto instanceof Contest contest) {
            getComune().getContestController().eliminaContest(contest);
        } else {
            getComune().getContenutoController().eliminaContenuto(contenuto);
        }
    }

    public void rimuoviTappa(Itinerario itinerario, PuntoInteresse puntoInteresse){
        getComune().getContenutoController().rimuoviTappa(itinerario, puntoInteresse);
    }

}
