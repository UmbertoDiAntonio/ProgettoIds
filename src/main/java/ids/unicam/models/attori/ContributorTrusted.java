package ids.unicam.models.attori;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;

public class ContributorTrusted extends Contributor{
    public ContributorTrusted(Comune comune) {
        super(comune);
    }

    @Override
    public void addPuntoInteresse(PuntoInteresse puntoInteresse){
       super.addPuntoInteresse(puntoInteresse);
       puntoInteresse.setApproved(true);
    }
    @Override
    public void addMateriale(PuntoInteresse puntoInteresse, Materiale materiale){
        super.addMateriale(puntoInteresse,materiale);
        materiale.setApproved(true);
    }
    @Override
    public void creaItinerario(PuntoInteresse puntoInteresseIniziale){
        super.creaItinerario(puntoInteresseIniziale);
       puntoInteresseIniziale.setApproved(true);
    }
    @Override
    public void aggiungiTappaItinerario(Itinerario itinerario, PuntoInteresse puntoInteresse){
        super.aggiungiTappaItinerario(itinerario,puntoInteresse);
        puntoInteresse.setApproved(true);
    }
}
