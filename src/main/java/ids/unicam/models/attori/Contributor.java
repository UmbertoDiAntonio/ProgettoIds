package ids.unicam.models.attori;

import ids.unicam.Comune;
import ids.unicam.models.Tempo;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Observer;
import ids.unicam.utilites.Stato;
import jakarta.persistence.Entity;
import jakarta.persistence.OneToOne;
import org.jetbrains.annotations.Nullable;

import java.util.GregorianCalendar;

@Entity
public class Contributor extends TuristaAutenticato implements Observer {
    @OneToOne
    private Comune comune = null;

    public Contributor() {

    }


    Comune getComune() {
        return comune;
    }


    protected Contributor(Comune comune, TuristaAutenticato turistaAutenticato) {
        super(turistaAutenticato.getNome(), turistaAutenticato.getCognome(), turistaAutenticato.getDataNascita(), turistaAutenticato.getPassword(), turistaAutenticato.getUsername());
        this.comune = comune;
    }

    protected Contributor(Comune comune, String name, String surname, GregorianCalendar dateBirthday, String password, String username) {
        super(name, surname, dateBirthday, password, username);
        this.comune = comune;
    }

    /**
     * Se il punto di interesse si trova all'interno del territorio del comune del Contributor invia la richiesta di aggiunta al controller di contenuti associato al comune
     *
     * @param puntoInteresse il punto di interesse da aggiungere al comune del contributor
     * @return true se il punto di interesse è stato aggiunto, false se il punto non fa parte del comune
     */
    public boolean aggiungiPuntoInteresse(PuntoInteresse puntoInteresse) {
        if (comune.verificaCoordinateComune(puntoInteresse)) {
            comune.getContenutoController().aggiungiPuntoInteresse(puntoInteresse);
            return true;
        }
        return false;
    }

    /**
     * Se il punto di interesse si trova all'interno del territorio del comune del Contributor invia la richiesta di aggiunta al controller di contenuti associato al comune
     *
     * @param puntoInteresse il punto di interesse del comune in cui aggiungere il materiale
     * @param materiale      il materiale da aggiungere
     * @return true se il punto di interesse è stato aggiunto, false se il punto non fa parte del comune
     */
    public boolean aggiungiMateriale(PuntoInteresse puntoInteresse, Materiale materiale) {
        if (comune.verificaCoordinateComune(puntoInteresse)) {
            comune.getContenutoController().aggiungiMateriale(puntoInteresse, materiale);
            return true;
        }
        return false;
    }

    /**
     * Se ogni punto di interesse che dovrebbe far parte dell'itinerario da creare fa parte del comune e è approvato invia la richiesta di creazione al controller di contenuti associato al comune del Contributor
     *
     * @param nome           nome dell'itinerario da creare
     * @param puntiInteresse punti di interesse da aggiungere all'itinerario
     * @return l'itinerario appena creato o null se un punto è fuori dal territorio del comune o non approvato
     */
    public @Nullable Itinerario creaItinerario(String nome, PuntoInteresse... puntiInteresse) {
        for (PuntoInteresse puntoInteresse : puntiInteresse) {
            if (!comune.verificaCoordinateComune(puntoInteresse) || !puntoInteresse.getStato().getApprovato()) {
                return null;
            }
        }
        return comune.getContenutoController().creaItinerario(nome, puntiInteresse);
    }

    /**
     * Se il punto da aggiungere fa parte del territorio del comune del contributor lo aggiunge come tappa all'itinerario richiamando il metodo del controller di contenuti associato al Comune del Contributor
     *
     * @param itinerario     l'itinerario a cui aggiungere la tappa
     * @param puntoInteresse il punto di interesse da aggiungere come tappa all'itinerario
     * @return true se il punto è stato aggiunto, false se l'aggiunta è fallita (il punto è fuori dal territorio del comune)
     */
    public boolean aggiungiTappaItinerario(Itinerario itinerario, PuntoInteresse puntoInteresse) {
        if (comune.verificaCoordinateComune(puntoInteresse)) {
            comune.getContenutoController().aggiungiTappa(itinerario, puntoInteresse);
            return true;
        }
        return false;
    }

    /**
     * @param contenuto il Contenuto di cui stiamo modificando la scadenza
     * @param giorni    la nuova scadenza
     */
    //TODO completar metodo
    public void aggiungiScadenzaContenuto(Contenuto contenuto, Tempo giorni) {
        contenuto.setScadenza(giorni);
        throw new UnsupportedOperationException();

    }

    @Override
    public void riceviNotifica(Stato eventType, PuntoInteresse puntoInteresse) {
        switch (eventType) {
            case APPROVED ->
                    System.out.println("Il tuo " + puntoInteresse.mostraInformazioniGeneriche() + " e' stato approvato");
            case NOT_APPROVED ->
                    System.out.println("Il tuo " + puntoInteresse.mostraInformazioniGeneriche() + " non e' stato approvato");
        }
    }

    @Override
    public void riceviNotifica(Stato eventType, Materiale materiale) {
        switch (eventType) {
            case APPROVED -> System.out.println("Il tuo contenuto relativo al punto di interesse " + materiale.get() + " e' stato approvato");
            case NOT_APPROVED ->  System.out.println("Il tuo contenuto relativo al punto di interesse " + materiale.get() + " non e' stato approvato");
        }
    }
}
