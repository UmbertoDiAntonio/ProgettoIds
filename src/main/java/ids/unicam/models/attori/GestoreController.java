package ids.unicam.models.attori;

import ids.unicam.Comune;
import ids.unicam.DataBase.GestoreDatabase;
import ids.unicam.DataBase.ModificaTabelleDatabase;
import ids.unicam.Exception.ConnessioneFallitaException;
import ids.unicam.Exception.RegistrazioneException;
import ids.unicam.controller.UtentiController;
import ids.unicam.models.Ruolo;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.GregorianCalendar;

import static ids.unicam.Main.logger;

@Component
public class GestoreController {

    private final UtentiController utentiController = new UtentiController();

    public UtentiController getUtentiController() {
        return utentiController;
    }

    private final GestoreDatabase gestoreDatabase;

    @Autowired
    public GestoreController(GestoreDatabase gestoreDatabase) {
        this.gestoreDatabase = gestoreDatabase;
    }

    /**
     * Modifica il ruolo di un contributor all'interno del comune
     *
     * @param contributor il contributor a cui cambiare ruolo
     * @param ruolo       il nuovo ruolo
     */
    public void cambiaRuolo(Contributor contributor, @NotNull Ruolo ruolo) {
        Comune comune = contributor.getComune();
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
        Comune comune = contributor.getComune();
        switch (contributor) {
            case Curatore curatore -> comune.getCuratori().remove(curatore);
            case ContributorAutorizzato contributorAutorizzato ->
                    comune.getContributorAutorizzati().remove(contributorAutorizzato);
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
        // Crea un nuovo oggetto TuristaAutenticato
        TuristaAutenticato nuovoTurista = new TuristaAutenticato(nome, cognome, birthday, password, username);

        // Aggiungi il nuovo turista alla lista dei turisti nel controller degli utenti
        utentiController.getTuristi().add(nuovoTurista);
        // Ottieni una connessione al database
        Connection connection = gestoreDatabase.getConnessioneDatabase().connessioneAlDatabase();
        if(connection==null){
            logger.error("Creazione Turista fallita, impossibile stabiliere una connessione con il DB");
            //TODO trow oppure?
        }
        try {
            gestoreDatabase.getCreazioneTabelleDatabase().inizializzaDatabase();
        } catch (SQLException e) {
            throw new RuntimeException(e);
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }
        // Inserisci i dati del nuovo turista nel database
        gestoreDatabase.getModificaTabelleDatabase().aggiungiTuristaAlDatabase(nuovoTurista);
        //gestoreDatabase.getModificaTabelleDatabase().rimuoviTuristaAlDatabase(nuovoTurista);

    }

    public Contributor registraContributor(Comune comune, String nome, String cognome, GregorianCalendar birthday, String password, String username) {
        Contributor contributor = new Contributor(comune, nome, cognome, birthday, password, username);
        comune.getContributors().add(contributor);
        return contributor;
        //TODO aggiungere al database
    }


}
