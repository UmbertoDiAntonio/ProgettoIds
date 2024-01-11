package ids.unicam.models.attori;

import ids.unicam.controller.GestoreController;
import ids.unicam.models.Comune;
import ids.unicam.models.Gradi;

public class GestorePiattaforma{

    public void promuovi(Comune comune, Contributor contributor, Gradi grado){
        GestoreController.upgrade(comune,contributor,grado);
    }

}
