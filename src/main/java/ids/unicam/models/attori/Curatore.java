package ids.unicam.models.attori;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Materiale;

public class Curatore extends ContributorTrusted {

    public Curatore(Comune comune,Contributor contributor) {
        super(comune,contributor);
    }

    public void share(Contenuto contenuto){
        //TODO
    }
    public void approva(Materiale materiale){
        materiale.setApproved(true);
        materiale.getOwner().addMateriale(materiale);

    }
    public void approva(Contenuto contenuto) {
        contenuto.setApproved(true);
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
