package ids.unicam.models.attori;

import ids.unicam.models.*;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;

public class Curatore extends ContributorTrusted {

    public Curatore(Comune comune,Contributor contributor) {
        super(comune,contributor);
    }

    public void share(Contenuto contenuto){
        //TODO
    }
    public void approva(Materiale materiale){
        getComune().getContenutoController().getWaitingMaterials().remove(materiale);
        materiale.setApproved(true);
        materiale.getOwner().addMateriale(materiale);

    }
    public void approva(Contenuto contenuto) {
        contenuto.setApproved(true);
        getComune().getContenutoController().getWaiting().remove(contenuto);
    }


    public void delete(Contenuto contenuto){
        getComune().getContenutoController().deleteContenuto(contenuto);
    }

    @Override
    public boolean logOut(){
        //TODO
        return true;
    }
}
