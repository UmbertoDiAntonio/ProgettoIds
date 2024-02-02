package ids.unicam.boundary;

import ids.unicam.Comune;
import ids.unicam.controller.UtentiController;

import java.util.GregorianCalendar;

public class MenuTurista {
    UtentiController controller;

    public MenuTurista(UtentiController controller) {
        this.controller = controller;
    }

    public void registra(Comune comune, int tipo, String nome, String cognome, GregorianCalendar birthday, String password, String username){
        comune.getGestorePiattaforma().getGestoreController().registra(comune,tipo,nome,cognome,birthday,password,username);
    }
}
