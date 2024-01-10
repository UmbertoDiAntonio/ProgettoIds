package ids.unicam.models.attori;

import ids.unicam.models.*;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;

public class Curatore extends ContributorTrusted {
    private Comune comune;

    public Curatore(Comune comune) {
        super(comune);
    }

    public void share(Contenuto contenuto){
        //TODO
    }
    public void approva(Materiale materiale){
        controller.getWaitingMaterials().remove(materiale);
        materiale.setPending(false);
        materiale.getOwner().addMateriale(materiale);

    }
    public void approva(Itinerario itinerario){
        controller.getWaitingItinerario().remove(itinerario);
        comune.getContenuti().add(itinerario);
        itinerario.setApproved(true);
    }
    public void approva(PuntoInteresse puntoInteresse){
        controller.getWaitingPoints().remove(puntoInteresse);
        comune.getContenuti().add(puntoInteresse);
        puntoInteresse.setApproved(true);
    }
    public void delete(Contenuto contenuto){
        controller.deleteContenuto(contenuto);
    }

    @Override
    public boolean logOut(){
        //TODO
        return true;
    }
}
