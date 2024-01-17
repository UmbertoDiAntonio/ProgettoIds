package ids.unicam.boundary;

import ids.unicam.controller.UtentiController;
import ids.unicam.models.Comune;

import java.util.Date;

public class MenuTurista {
    UtentiController controller;

    public MenuTurista(UtentiController controller) {
        this.controller = controller;
    }

    public void registra(Comune comune, int tipo, String nome, String cognome, Date birthday, String password, String username){
        comune.getGestorePiattaforma().getGestoreController().registra(comune,tipo,nome,cognome,birthday,password,username);
    }
}
