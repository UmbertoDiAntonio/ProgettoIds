package ids.unicam.models.attori;

import ids.unicam.models.Comune;
import ids.unicam.models.Tempo;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;

public class Contributor extends TuristaLoggato {
    protected final Comune comune;

    public void addPuntoInteresse(PuntoInteresse puntoInteresse){
        comune.getContenutoController().addPunto(puntoInteresse);
    }
    public void addMateriale(PuntoInteresse puntoInteresse, Materiale materiale){
        comune.getContenutoController().addMaterialeTo(puntoInteresse,materiale);
    }
    public void creaItinerario(PuntoInteresse puntoInteresseIniziale){
        comune.getContenutoController().creaItinerario(puntoInteresseIniziale);
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

    public Contributor(Turista turista,Comune comune) {
        super(turista);
        this.comune=comune;
    }
}
