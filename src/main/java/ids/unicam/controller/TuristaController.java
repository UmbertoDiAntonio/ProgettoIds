package ids.unicam.controller;

public class TuristaController {
    public void registra(int tipo) {
        switch (tipo) {
            case 0:
                registraTurista();
                break;
            case 1:
                registraContributor();
                break;
            default:
                //TODO
                return;
        }

    }


    public void registraTurista() {
        //TODO
    }

    public void registraContributor() {
        //TODO
    }
}
