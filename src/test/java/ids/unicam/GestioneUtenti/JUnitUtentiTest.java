package ids.unicam.GestioneUtenti;

import ids.unicam.Comune;
import ids.unicam.models.Ruolo;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.Foto;
import ids.unicam.models.contenuti.POIFactory.AttivitaFactory;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import ids.unicam.utilites.Stato;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.util.GregorianCalendar;

import static org.junit.jupiter.api.Assertions.*;

public class JUnitUtentiTest {
    @Test
    public void generazioneUtenti() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("nome", gestorePiattaforma);

        gestorePiattaforma.getGestoreController().registraTurista("Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH,17), "pass", "user");
        gestorePiattaforma.getGestoreController().registraTurista("Paolo", "Giallo", new GregorianCalendar(2000, GregorianCalendar.MARCH,17), "pass", "user");

        gestorePiattaforma.getGestoreController().registraContributor(comune, "Giuseppe", "Oro", new GregorianCalendar(2000, GregorianCalendar.MARCH,17), "PASS", "user");
        assertEquals(1, comune.getContributors().size());

        gestorePiattaforma.promuovi(comune.getContributors().getFirst(), Ruolo.Curatore);

        assertEquals(0, comune.getContributors().size());
        assertEquals(1, comune.getCuratori().size());

        gestorePiattaforma.promuovi(comune.getCuratori().getFirst(), Ruolo.Contributor);

        assertEquals(1, comune.getContributors().size());
        assertEquals(0, comune.getCuratori().size());

        gestorePiattaforma.promuovi(comune.getContributors().getFirst(), Ruolo.ContributorTrusted);

        assertEquals(1, comune.getContributorAutorizzati().size());
        assertEquals(0, comune.getCuratori().size());

    }


    @Test
    public void aggiungiPreferito() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma);
        comune.getPosizione();
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH,17), "ciao", "mr");
        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH,17), "eroe", "AN2");
        TuristaAutenticato turistaAutenticato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();
        AttivitaFactory attivitaFactory = new AttivitaFactory(LocalDate.now());

        gestorePiattaforma.promuovi(contributor, Ruolo.ContributorTrusted);
        PuntoInteresse puntoInteresse = attivitaFactory.creaPoi("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));

        ContributorAutorizzato contributorAutorizzato = comune.getContributorAutorizzati().getFirst();
        contributorAutorizzato.aggiungiPuntoInteresse(puntoInteresse);
        assertEquals(0, turistaAutenticato.getPreferiti().size());
        turistaAutenticato.aggiungiPreferito(puntoInteresse);
        assertEquals(1, turistaAutenticato.getPreferiti().size());
    }

    @Test
    public void condividiContenuto() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma);
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH,17), "ciao", "mr");
        gestorePiattaforma.promuovi(contributor, Ruolo.Curatore);
        Curatore curatore = comune.getCuratori().getFirst();
        AttivitaFactory attivitaFactory = new AttivitaFactory(LocalDate.now());
        PuntoInteresse puntoInteresse = attivitaFactory.creaPoi("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));

        assertThrows(UnsupportedOperationException.class, () -> curatore.condividi(puntoInteresse));
        //TODO
    }

    @Test
    public void searchMethodTest() {
        Turista turista=new Turista();
        assertEquals(0,turista.search("empty").size());

        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma);
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH,17), "ciao", "mr");

        gestorePiattaforma.promuovi(contributor, Ruolo.Curatore);

        AttivitaFactory attivitaFactory = new AttivitaFactory(LocalDate.now());
        PuntoInteresse puntoInteresse = attivitaFactory.creaPoi("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));
        puntoInteresse.aggiungiTag("Edicola");
        contributor.aggiungiPuntoInteresse(puntoInteresse);
        Curatore curatore=comune.getCuratori().getFirst();
        assertEquals(0,turista.search("Edicola").size());
        curatore.valuta(puntoInteresse, Stato.APPROVED);
        assertEquals(1,turista.search("Edicola").size());

    }

    @Test
    public void aggiungiFoto() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma);
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH,17), "ciao", "mr");

        contributor.aggiungiPuntoInteresse(new AttivitaFactory(LocalDate.now()).creaPoi("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015)));
        PuntoInteresse puntoInteresse = comune.getContenutoController().getContenuti().getFirst();

        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH,17), "eroe", "AN2");
        TuristaAutenticato turistaAutenticato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();

        assertEquals(0, puntoInteresse.getListaMateriali().size());
        turistaAutenticato.aggiungiFoto(puntoInteresse, new Foto(turistaAutenticato));
        assertEquals(1, puntoInteresse.getListaMateriali().size());
        assertFalse(puntoInteresse.getListaMateriali().getFirst().getStato().getApprovato());
        gestorePiattaforma.promuovi(contributor, Ruolo.Curatore);
        Curatore curatore = comune.getCuratori().getFirst();
        curatore.valuta(puntoInteresse.getListaMateriali().getFirst(), Stato.APPROVED);
        assertTrue(puntoInteresse.getListaMateriali().getFirst().getStato().getApprovato());
    }
}