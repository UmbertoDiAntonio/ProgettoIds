package ids.unicam.models.attori;

import ids.unicam.controller.ContributorController;
import ids.unicam.models.Gradi;

public class GestorePiattaforma extends TuristaLoggato {
    public void promuovi(Contributor contributor, Gradi grado){
        ContributorController.upgrade(contributor,grado);
    }

    @Override
    public boolean logOut(){
        //TODO
        return true;
    }
}
