package ids.unicam.models.attori;

import ids.unicam.Comune;
import ids.unicam.models.contenuti.*;
import ids.unicam.utilites.Observer;
import ids.unicam.utilites.Stato;
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

    public void notifica(Stato eventType, PuntoInteresse puntoInteresse) {
        for (Observer listener : osservatori) {
            listener.riceviNotifica(eventType, puntoInteresse);
        }
    }

    
    public ArrayList<Observer> getOsservatori() {
        return osservatori;
    }

    public void notifica(Stato eventType, MaterialeGenerico materialeGenerico) {

        for (Observer listener : osservatori) {
            listener.riceviNotifica(eventType, materialeGenerico);
        }
    }

    protected Curatore(Comune comune, Contributor contributor) {
        super(comune, contributor);
    }

    public void condividi(ContenutoGenerico contenutoGenerico) {
        throw new UnsupportedOperationException(contenutoGenerico.getId()+"non può ancora essere condiviso");
        //TODO
    }

    /**
     * Valuta un Materiale,
     * notifica i subscriber
     *
     * @param materialeGenerico il materiale che si vuole valutare
     * @param approvato approvato o non approvato
     */
    public void valuta(MaterialeGenerico materialeGenerico, Stato approvato) {
        materialeGenerico.setStato(approvato);
        notifica(approvato, materialeGenerico);
    }

    /**
     * Valuta un punto di interesse, in caso di non approvazione lo rimuove dalla lista dei contenuti nel controller del comune associato,
     * notifica i subscriber
     *
     * @param puntoInteresse il punto di interesse che si vuole valutare
     * @param approvato      stato punto di interesse: approvato/non approvato
     */
    public void valuta(@NotNull PuntoInteresse puntoInteresse, Stato approvato) {
        puntoInteresse.setStato(approvato);
        if (approvato== Stato.NOT_APPROVED)
            getComune().getContenutoController().getContenuti().remove(puntoInteresse);

        notifica(approvato, puntoInteresse);
    }


    public void elimina(ContenutoGenerico contenutoGenerico) {
        if (contenutoGenerico instanceof Contest contest) {
            getComune().getContestController().eliminaContest(contest);
        } else {
            getComune().getContenutoController().eliminaContenuto(contenutoGenerico);
        }
    }
    public void elimina(MaterialeGenerico materialeDaEliminare) {//TODO se non gli passiamo anche il POI questa operazione porta via tempo
        for(PuntoInteresse poi: getComune().getContenutoController().getContenuti()){
            poi.getMateriali().remove(materialeDaEliminare);
        }
        for(Contest contest: getComune().getContestController().getContests()){
            contest.getMateriali().remove(materialeDaEliminare);
        }
    }

    public void rimuoviTappa(Itinerario itinerario, PuntoInteresse puntoInteresse){
        getComune().getContenutoController().rimuoviTappa(itinerario, puntoInteresse);
    }

}
