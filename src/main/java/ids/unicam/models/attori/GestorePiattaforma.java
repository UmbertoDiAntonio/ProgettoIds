package ids.unicam.models.attori;

import ids.unicam.models.Comune;
import ids.unicam.models.Gradi;

public class GestorePiattaforma{

    private final GestoreController gestoreController = new GestoreController();

    public void promuovi(Comune comune, Contributor contributor, Gradi grado){
        gestoreController.upgrade(comune,contributor,grado);
    }

    public GestoreController getGestoreController() {
        return gestoreController;
    }
}
