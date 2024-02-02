package ids.unicam.models.attori;

import ids.unicam.Comune;
import ids.unicam.Exception.RegisterException;
import ids.unicam.controller.UtentiController;
import ids.unicam.models.Ruolo;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.OneToOne;
import org.jetbrains.annotations.NotNull;

import java.util.Date;
import java.util.GregorianCalendar;


public class GestoreController {
    
    private final UtentiController utentiController = new UtentiController();
    
    @GeneratedValue
    private Long id;

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
                comune.getContributorTrusteds().add(new ContributorTrusted(comune, contributor));
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
            case ContributorTrusted contributorTrusted -> comune.getContributorTrusteds().remove(contributorTrusted);
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
                throw new RegisterException("Il tipo di registrazione richiesta non è valido");
        }
    }

    public void registraTurista(String nome, String cognome, GregorianCalendar birthday, String password, String username) {
        utentiController.getTuristi().add(new TuristaLoggato(nome, cognome, birthday, password, username));

        //TODO aggiungere al database

    }


    public Contributor registraContributor(Comune comune, String nome, String cognome, GregorianCalendar birthday, String password, String username) {
        Contributor contributor = new Contributor(comune, nome, cognome, birthday, password, username);
        comune.getContributors().add(contributor);
        return contributor;
        //TODO aggiungere al database
    }


    public void setId(Long id) {
        this.id = id;
    }

    public Long getId() {
        return id;
    }
}
