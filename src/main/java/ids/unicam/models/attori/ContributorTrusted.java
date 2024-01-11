package ids.unicam.models.attori;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;

import java.util.Arrays;

public class ContributorTrusted extends Contributor{
    public ContributorTrusted(Comune comune,Contributor contributor) {
        super(comune,contributor);
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
    public void creaItinerario(String nome,PuntoInteresse... puntoInteresse){
        super.creaItinerario(nome,puntoInteresse);
        Arrays.stream(puntoInteresse).toList().forEach(puntoInteresse1 -> puntoInteresse1.setApproved(true));
    }
    @Override
    public void aggiungiTappaItinerario(Itinerario itinerario, PuntoInteresse puntoInteresse){
        super.aggiungiTappaItinerario(itinerario,puntoInteresse);
        puntoInteresse.setApproved(true);
    }
}