package ids.unicam.controller;

public class TuristaController {
    private static long id=0;
    public static long generateID(){
        return id++;
    }
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
