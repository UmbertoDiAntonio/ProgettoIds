package ids.unicam.models.attori;

import ids.unicam.controller.ContenutoController;
import ids.unicam.models.*;

public class Contributor extends TuristaLoggato {
    private final ContenutoController controller=new ContenutoController();//TODO

    public void addPuntoInteresse(PuntoInteresse puntoInteresse){
        controller.addPunto(puntoInteresse);
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
}
