package ids.unicam.models.attori;

import ids.unicam.models.Comune;
import ids.unicam.models.Tempo;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.OSM.OSMRequester;
import ids.unicam.utilites.Punto;

import java.util.Date;

public class Contributor extends TuristaLoggato {
    private Comune comune;

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
    public Itinerario creaItinerario(String nome,PuntoInteresse... puntoInteresseIniziale){
        //TODO check puntiInteresse sono approvati
        for(PuntoInteresse puntoInteresse : puntoInteresseIniziale){
            if(!checkCoordinateComune(puntoInteresse)){
                return null;
            }
        }
        return comune.getContenutoController().creaItinerario(nome,puntoInteresseIniziale);
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

}
