package ids.unicam.models.attori;

import ids.unicam.models.Comune;
import ids.unicam.models.Tempo;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;

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

    public void addPuntoInteresse(PuntoInteresse puntoInteresse){
        comune.getContenutoController().addPunto(puntoInteresse);
    }
    public void addMateriale(PuntoInteresse puntoInteresse, Materiale materiale){
        comune.getContenutoController().addMaterialeTo(puntoInteresse,materiale);
    }
    public void creaItinerario(String nome,PuntoInteresse... puntoInteresseIniziale){
        Itinerario itinerario = new Itinerario(nome, puntoInteresseIniziale);
        comune.getContenutoController().creaItinerario(nome,puntoInteresseIniziale);
    }
    public void aggiungiTappaItinerario(Itinerario itinerario, PuntoInteresse puntoInteresse){
        comune.getContenutoController().addTappa(itinerario,puntoInteresse);
    }
    public void aggiungiScadenzaContenuto(Contenuto contenuto, Tempo giorni){
        contenuto.setScadenza(giorni);

    }
    @Override
    public boolean logOut(){
        //TODO
        return true;
    }


}
