package ids.unicam.models.attori;

import ids.unicam.models.Ruolo;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.OneToOne;

@Entity
public class GestorePiattaforma{

    //TODO è strano che tutto ciò che faccia sia solo chiamare il suo controller, senza avere alcun dato o logica
    @OneToOne
    private final GestoreController gestoreController = new GestoreController();
    @Id
    @GeneratedValue
    private Long id;

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


    public void setId(Long id) {
        this.id = id;
    }

    public Long getId() {
        return id;
    }
}
