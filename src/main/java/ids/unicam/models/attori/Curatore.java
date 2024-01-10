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
        materiale.setApproved(true);
        materiale.getOwner().addMateriale(materiale);

    }
    public void approva(Contenuto contenuto) {
        contenuto.setApproved(true);
        controller.getWaiting().remove(contenuto);
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
