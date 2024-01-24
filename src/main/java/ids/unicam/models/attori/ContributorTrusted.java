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
    public boolean addPuntoInteresse(PuntoInteresse puntoInteresse){
        if(super.addPuntoInteresse(puntoInteresse)){
            puntoInteresse.setApproved(true);
            return true;
        }
        return false;
    }
    @Override
    public boolean addMateriale(PuntoInteresse puntoInteresse, Materiale materiale){

        if(super.addMateriale(puntoInteresse,materiale)){
            materiale.setApproved(true);
            return true;
        }
        return false;
    }

    @Override
    public Itinerario creaItinerario(String nome,PuntoInteresse... puntiInteresse){
        return super.creaItinerario(nome, puntiInteresse);
        //Arrays.stream(puntoInteresse).toList().forEach(puntoInteresse1 -> puntoInteresse1.setApproved(true));
    }

}