package ids.unicam.controller;

import ids.unicam.models.Comune;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.TuristaLoggato;

import java.util.Date;

public class UtentiController {
    private static long id=0;
    public static String generateID(){
        return String.format("%04d",id++);
    }




    public void registra(Comune comune,int tipo, String nome, String cognome, Date birthday, String password, String username) {
        switch (tipo) {
            case 0:
                registraTurista(comune,nome,cognome,birthday,password,username);
                break;
            case 1:
                registraContributor(comune,nome,cognome,birthday,password,username);
                break;
            default:
                //TODO
                return;
        }

    }


    public void registraTurista(Comune comune, String nome, String cognome, Date birthday, String password, String username) {
        //TODO controlli validità dati
        comune.getTuristi().add( new TuristaLoggato(nome,cognome,birthday,password,username));

        //aggiungere al database

    }

    public void registraContributor(Comune comune,String nome, String cognome, Date birthday, String password, String username) {
        comune.getContributors().add(new Contributor(comune,nome,cognome,birthday,password,username));
        //TODO
    }
}
