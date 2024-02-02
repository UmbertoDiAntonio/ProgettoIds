package ids.unicam.models.attori;

import ids.unicam.models.Ruolo;


public class GestorePiattaforma{

    //TODO è strano che tutto ciò che faccia sia solo chiamare il suo controller, senza avere alcun dato o logica

    private final GestoreController gestoreController = new GestoreController();

    public GestoreController getGestoreController() {
        return gestoreController;
    }

    /**
     * Invia la richiesta di cambiare grado al contributor col nuovo grado
     * @param contributor il contributor a cui cambiare grado
     * @param grado il nuovo grado
     */
    public void promuovi(Contributor contributor, Ruolo grado){ //TODO rinominare
        gestoreController.cambiaRuolo(contributor,grado);
    }



}
