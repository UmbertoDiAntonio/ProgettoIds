package ids.unicam.models.attori;

import ids.unicam.OSM.OSMRequester;
import ids.unicam.models.Comune;
import ids.unicam.models.Tempo;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Observer;
import ids.unicam.utilites.Punto;

import java.util.Date;

public class Contributor extends TuristaLoggato implements Observer {
    private final Comune comune;

    public Comune getComune() {
        return comune;
    }

    public Contributor(Comune comune, TuristaLoggato turistaLoggato){
        super(turistaLoggato.getName(),turistaLoggato.getSurname(),turistaLoggato.getDateBirthday(),turistaLoggato.getPassword(), turistaLoggato.getUsername());
        this.comune=comune;
    }
    public Contributor(Comune comune,String name, String surname, Date dateBirthday, String password, String username) {
        super(name, surname, dateBirthday, password, username);
        this.comune=comune;
    }

    public boolean addPuntoInteresse(PuntoInteresse puntoInteresse){
        if(checkCoordinateComune(puntoInteresse)){
            comune.getContenutoController().addPunto(puntoInteresse);
            return true;
        }
        return false;
    }
    public boolean addMateriale(PuntoInteresse puntoInteresse, Materiale materiale){
        if(checkCoordinateComune(puntoInteresse)){
            comune.getContenutoController().addMaterialeTo(puntoInteresse,materiale);
            return true;
        }
        return false;
    }
    public Itinerario creaItinerario(String nome,PuntoInteresse... puntiInteresse){
        for(PuntoInteresse puntoInteresse : puntiInteresse){
            if(!checkCoordinateComune(puntoInteresse) && puntoInteresse.isApproved() ){
                return null;
            }
        }
        return comune.getContenutoController().creaItinerario(nome,puntiInteresse);
    }

    public boolean aggiungiTappaItinerario(Itinerario itinerario, PuntoInteresse puntoInteresse){
        if(checkCoordinateComune(puntoInteresse)){
            comune.getContenutoController().addTappa(itinerario,puntoInteresse);
            return true;
        }
        return false;
    }
    public void aggiungiScadenzaContenuto(Contenuto contenuto, Tempo giorni){
        contenuto.setScadenza(giorni);

    }
    @Override
    public boolean logOut(){
        //TODO
        return true;
    }

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
            System.out.println("Il tuo contenuto relativo al punto di interesse " + materiale.getOwner().getNome() + " è stato approvato");
        } else {
            System.out.println("Il tuo contenuto relativo al punto di interesse " + materiale.getOwner().getNome() + " non è stato approvato");
        }
    }
}
