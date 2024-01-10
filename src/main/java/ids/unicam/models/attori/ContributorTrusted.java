package ids.unicam.models.attori;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.PuntoInteresse;

public class ContributorTrusted extends Contributor{
    public ContributorTrusted(Comune comune) {
        super(comune);
    }

    @Override
    public void addPuntoInteresse(PuntoInteresse puntoInteresse){
        controller.addPunto(puntoInteresse,true);
    }
}
