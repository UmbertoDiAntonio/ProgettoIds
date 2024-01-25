package ids.unicam.models.attori;

import ids.unicam.controller.UtentiController;
import ids.unicam.models.Comune;
import ids.unicam.models.Gradi;
import org.jetbrains.annotations.NotNull;

import java.util.Date;

public class GestoreController {


    private final UtentiController utentiController = new UtentiController();

    public void upgrade(Comune comune, Contributor contributor, Gradi grado) {
        switch (grado) {
            case Curatore -> {
                comune.getCuratori().add(new Curatore(comune, contributor));
                removeOldRank(comune, contributor);
            }
            case Animatore -> {
                comune.getAnimatori().add(new Animatore(contributor));
                removeOldRank(comune, contributor);
            }
            case ContributorTrusted -> {
                removeOldRank(comune, contributor);
                comune.getContributorTrusteds().add(new ContributorTrusted(comune, contributor));
            }
            default -> {
                comune.getContributors().add(new Contributor(comune, contributor));
                removeOldRank(comune, contributor);
            }
        }

    }

    private void removeOldRank(Comune comune, @NotNull Contributor contributor) {
        switch (contributor) {
            case Curatore curatore -> comune.getCuratori().remove(curatore);
            case ContributorTrusted contributorTrusted -> comune.getContributorTrusteds().remove(contributorTrusted);
            case Animatore animatore -> comune.getAnimatori().remove(animatore);
            case Contributor contributor1 -> comune.getContributors().remove(contributor1);
        }
    }

    public void registra(Comune comune, int tipo, String nome, String cognome, Date birthday, String password, String username) {
        switch (tipo) {
            case 0:
                registraTurista(nome, cognome, birthday, password, username);
                break;
            case 1:
                registraContributor(comune, nome, cognome, birthday, password, username);
                break;
            default:
                //TODO
        }

    }

    public void registraTurista(String nome, String cognome, Date birthday, String password, String username) {
        utentiController.getTuristi().add(new TuristaLoggato(nome, cognome, birthday, password, username));

        //TODO aggiungere al database

    }


    public Contributor registraContributor(Comune comune, String nome, String cognome, Date birthday, String password, String username) {
        Contributor contributor = new Contributor(comune, nome, cognome, birthday, password, username);
        comune.getContributors().add(contributor);
        return contributor;
        //TODO aggiungere al database
    }

    public UtentiController getUtentiController() {
        return utentiController;
    }
}