package ids.unicam.models.attori;

import ids.unicam.controller.ContenutoController;
import ids.unicam.models.*;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;

public class Contributor extends TuristaLoggato {
    protected final ContenutoController controller;//TODO

    public void addPuntoInteresse(PuntoInteresse puntoInteresse){
        controller.addPunto(puntoInteresse,false);
    }
    public void addMateriale(PuntoInteresse puntoInteresse, Materiale materiale){
        controller.addMaterialeTo(puntoInteresse,materiale);
    }
    public void creaItinerario(PuntoInteresse puntoInteresseIniziale){
        controller.creaItinerario(puntoInteresseIniziale);
    }
    public void aggiungiTappaItinerario(Itinerario itinerario, PuntoInteresse puntoInteresse){
        controller.addTappa(itinerario,puntoInteresse);
    }
    public void aggiungiScadenzaContenuto(Contenuto contenuto, Tempo giorni){
        contenuto.setScadenza(giorni);

    }
    @Override
    public boolean logOut(){
        //TODO
        return true;
    }

    public Contributor(Comune comune) {
        this.controller=comune.getContenutoController();
    }
}
