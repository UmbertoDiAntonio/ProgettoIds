package ids.unicam.boundary;

import ids.unicam.controller.TuristaController;

public class MenuTurista {
    TuristaController controller;

    public MenuTurista(TuristaController controller) {
        this.controller = controller;
    }

    public void registra(int tipo){
        controller.registra(tipo);
    }
}
