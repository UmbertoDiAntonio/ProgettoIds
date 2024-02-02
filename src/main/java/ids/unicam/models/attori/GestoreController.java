package ids.unicam.models.attori;

import ids.unicam.Comune;
import ids.unicam.Exception.RegistrazioneException;
import ids.unicam.controller.UtentiController;
import ids.unicam.models.Ruolo;
import org.jetbrains.annotations.NotNull;

import java.util.GregorianCalendar;


public class GestoreController {
    
    private final UtentiController utentiController = new UtentiController();

    public UtentiController getUtentiController() {
        return utentiController;
    }

    /**
     * Modifica il ruolo di un contributor all'interno del comune
     *
     * @param contributor il contributor a cui cambiare ruolo
     * @param ruolo il nuovo ruolo
     */
    public void cambiaRuolo(Contributor contributor, @NotNull Ruolo ruolo) {
        Comune comune=contributor.getComune();
        switch (ruolo) {
            case Curatore -> {
                comune.getCuratori().add(new Curatore(comune, contributor));
                rimuoviVecchioRuolo(contributor);
            }
            case Animatore -> {
                comune.getAnimatori().add(new Animatore(contributor));
                rimuoviVecchioRuolo(contributor);
            }
            case ContributorTrusted -> {
                rimuoviVecchioRuolo(contributor);
                comune.getContributorAutorizzati().add(new ContributorAutorizzato(comune, contributor));
            }
            default -> {
                comune.getContributors().add(new Contributor(comune, contributor));
                rimuoviVecchioRuolo(contributor);
            }
        }

    }

    private void rimuoviVecchioRuolo(@NotNull Contributor contributor) {
        Comune comune=contributor.getComune();
        switch (contributor) {
            case Curatore curatore -> comune.getCuratori().remove(curatore);
            case ContributorAutorizzato contributorAutorizzato -> comune.getContributorAutorizzati().remove(contributorAutorizzato);
            case Animatore animatore -> comune.getAnimatori().remove(animatore);
            case Contributor contributor1 -> comune.getContributors().remove(contributor1);
        }
    }

    public void registra(Comune comune, int tipo, String nome, String cognome, GregorianCalendar birthday, String password, String username) {
        switch (tipo) {
            case 0:
                registraTurista(nome, cognome, birthday, password, username);
                break;
            case 1:
                registraContributor(comune, nome, cognome, birthday, password, username);
                break;
            default:
                throw new RegistrazioneException("Il tipo di registrazione richiesta non è valido");
        }
    }

    public void registraTurista(String nome, String cognome, GregorianCalendar birthday, String password, String username) {
        utentiController.getTuristi().add(new TuristaAutenticato(nome, cognome, birthday, password, username));

        //TODO aggiungere al database

    }


    public Contributor registraContributor(Comune comune, String nome, String cognome, GregorianCalendar birthday, String password, String username) {
        Contributor contributor = new Contributor(comune, nome, cognome, birthday, password, username);
        comune.getContributors().add(contributor);
        return contributor;
        //TODO aggiungere al database
    }

}
