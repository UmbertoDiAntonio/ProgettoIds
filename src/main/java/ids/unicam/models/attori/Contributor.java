package ids.unicam.models.attori;

import ids.unicam.OSM.OSMRequester;
import ids.unicam.Comune;
import ids.unicam.models.Tempo;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Observer;
import ids.unicam.utilites.Punto;
import org.jetbrains.annotations.Nullable;

import java.util.Date;

public class Contributor extends TuristaLoggato implements Observer {
    private final Comune comune;

    public final Comune getComune() {
        return comune;
    }


    protected Contributor(Comune comune, TuristaLoggato turistaLoggato){
        super(turistaLoggato.getName(),turistaLoggato.getSurname(),turistaLoggato.getDateBirthday(),turistaLoggato.getPassword(), turistaLoggato.getUsername());
        this.comune=comune;
    }
    protected Contributor(Comune comune,String name, String surname, Date dateBirthday, String password, String username) {
        super(name, surname, dateBirthday, password, username);
        this.comune=comune;
    }

    /**
     * Se il punto di interesse si trova all'interno del territorio del comune del Contributor invia la richiesta di aggiunta al controller di contenuti associato al comune
     *
     * @param puntoInteresse il punto di interesse da aggiungere al comune del contributor
     * @return true se il punto di interesse è stato aggiunto, false se il punto non fa parte del comune
     */
    public boolean addPuntoInteresse(PuntoInteresse puntoInteresse){
        if(checkCoordinateComune(puntoInteresse)){
            comune.getContenutoController().addPunto(puntoInteresse);
            return true;
        }
        return false;
    }

    /**
     * Se il punto di interesse si trova all'interno del territorio del comune del Contributor invia la richiesta di aggiunta al controller di contenuti associato al comune
     *
     * @param puntoInteresse il punto di interesse del comune in cui aggiungere il materiale
     * @param materiale il materiale da aggiungere
     * @return true se il punto di interesse è stato aggiunto, false se il punto non fa parte del comune
     */
    public boolean addMateriale(PuntoInteresse puntoInteresse, Materiale materiale){
        if(checkCoordinateComune(puntoInteresse)){
            comune.getContenutoController().addMaterialeTo(puntoInteresse,materiale);
            return true;
        }
        return false;
    }

    /**
     * Se ogni punto di interesse che dovrebbe far parte dell'itinerario da creare fa parte del comune e è approvato invia la richiesta di creazione al controller di contenuti associato al comune del Contributor
     *
     * @param nome nome dell'itinerario da creare
     * @param puntiInteresse punti di interesse da aggiungere all'itinerario
     * @return l'itinerario appena creato o null se un punto è fuori dal territorio del comune o non approvato
     */
    public @Nullable Itinerario creaItinerario(String nome, PuntoInteresse... puntiInteresse){
        for(PuntoInteresse puntoInteresse : puntiInteresse){
            if(!checkCoordinateComune(puntoInteresse) && puntoInteresse.isApproved() ){
                return null;
            }
        }
        return comune.getContenutoController().creaItinerario(nome,puntiInteresse);
    }

    /**
     * Se il punto da aggiungere fa parte del territorio del comune del contributor lo aggiunge come tappa all'itinerario richiamando il metodo del controller di contenuti associato al Comune del Contributor
     * @param itinerario l'itinerario a cui aggiungere la tappa
     * @param puntoInteresse il punto di interesse da aggiungere come tappa all'itinerario
     * @return true se il punto è stato aggiunto, false se l'aggiunta è fallita (il punto è fuori dal territorio del comune)
     */
    //TODO testare
    public boolean aggiungiTappaItinerario(Itinerario itinerario, PuntoInteresse puntoInteresse){
        if(checkCoordinateComune(puntoInteresse)){
            comune.getContenutoController().addTappa(itinerario,puntoInteresse);
            return true;
        }
        return false;
    }

    /**
     *
     * @param contenuto
     * @param giorni
     */
    //TODO testare/fare
    public void aggiungiScadenzaContenuto(Contenuto contenuto, Tempo giorni){
        contenuto.setScadenza(giorni);

    }

    //TODO va qui? direi di no
    public final boolean checkCoordinateComune(PuntoInteresse puntoInteresse){
        String nomeComune = OSMRequester.getComuneAt(new Punto(puntoInteresse.getPt().getLatitudine(),puntoInteresse.getPt().getLongitudine()));
        if(nomeComune != null) {
            return nomeComune.equalsIgnoreCase(comune.getNome());
        }
        return false;
    }

    @Override
    public void riceviNotifica(boolean eventType, PuntoInteresse puntoInteresse) {
        if (eventType) {
            System.out.println("Il tuo " + puntoInteresse.getGeneralInfo() + " è stato approvato");
        } else {
            System.out.println("Il tuo " + puntoInteresse.getGeneralInfo() + " non è stato approvato");
        }
    }

    @Override
    public void riceviNotifica(boolean eventType, Materiale materiale) {
        if (eventType) {
            System.out.println("Il tuo contenuto relativo al punto di interesse " + materiale.getId() + " è stato approvato");
        } else {
            System.out.println("Il tuo contenuto relativo al punto di interesse " + materiale.getId() + " non è stato approvato");
        }
    }
}
