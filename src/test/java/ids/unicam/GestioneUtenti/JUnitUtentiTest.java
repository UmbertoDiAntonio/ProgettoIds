package ids.unicam.GestioneUtenti;

import ids.unicam.Comune;
import ids.unicam.models.Ruolo;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.Foto;
import ids.unicam.models.contenuti.POIFactory.AttivitaFactory;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

public class JUnitUtentiTest {
    @Test
    public void generazioneUtenti() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("nome", gestorePiattaforma);

        gestorePiattaforma.getGestoreController().registraTurista("Mario", "Rossi", new Date(), "pass", "user");
        gestorePiattaforma.getGestoreController().registraTurista("Paolo", "Giallo", new Date(), "pass", "user");

        gestorePiattaforma.getGestoreController().registraContributor(comune, "Giuseppe", "Oro", new Date(), "PASS", "user");
        assertEquals(1, comune.getContributors().size());

        gestorePiattaforma.promuovi(comune.getContributors().getFirst(), Ruolo.Curatore);

        assertEquals(0, comune.getContributors().size());
        assertEquals(1, comune.getCuratori().size());

        gestorePiattaforma.promuovi(comune.getCuratori().getFirst(), Ruolo.Contributor);

        assertEquals(1, comune.getContributors().size());
        assertEquals(0, comune.getCuratori().size());

        gestorePiattaforma.promuovi(comune.getContributors().getFirst(), Ruolo.ContributorTrusted);

        assertEquals(1, comune.getContributorTrusteds().size());
        assertEquals(0, comune.getCuratori().size());

    }


    @Test
    public void aggiungiPreferito() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma);
        comune.getPosizione();
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new Date(), "ciao", "mr");
        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new Date(), "eroe", "AN2");
        TuristaLoggato turistaLoggato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();
        AttivitaFactory attivitaFactory = new AttivitaFactory(LocalDate.now());

        gestorePiattaforma.promuovi(contributor, Ruolo.ContributorTrusted);
        PuntoInteresse puntoInteresse = attivitaFactory.creaPoi("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));

        ContributorTrusted contributorTrusted = comune.getContributorTrusteds().getFirst();
        contributorTrusted.addPuntoInteresse(puntoInteresse);
        assertEquals(0, turistaLoggato.getFavourites().size());
        turistaLoggato.addFavourites(puntoInteresse);
        assertEquals(1, turistaLoggato.getFavourites().size());
    }

    @Test
    public void condividiContenuto() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma);
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new Date(), "ciao", "mr");
        gestorePiattaforma.promuovi(contributor, Ruolo.Curatore);
        Curatore curatore = comune.getCuratori().getFirst();
        AttivitaFactory attivitaFactory = new AttivitaFactory(LocalDate.now());
        PuntoInteresse puntoInteresse = attivitaFactory.creaPoi("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));

        assertThrows(UnsupportedOperationException.class, () -> curatore.share(puntoInteresse));
        //TODO
    }

    @Test
    public void searchMethodTest() {
        Turista turista=new Turista();
        assertEquals(0,turista.search("empty").size());

        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma);
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new Date(), "ciao", "mr");

        gestorePiattaforma.promuovi(contributor, Ruolo.Curatore);

        AttivitaFactory attivitaFactory = new AttivitaFactory(LocalDate.now());
        PuntoInteresse puntoInteresse = attivitaFactory.creaPoi("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));
        puntoInteresse.addTag("Edicola");
        contributor.addPuntoInteresse(puntoInteresse);
        Curatore curatore=comune.getCuratori().getFirst();
        assertEquals(0,turista.search("Edicola").size());
        curatore.valuta(puntoInteresse,true);
        assertEquals(1,turista.search("Edicola").size());

    }

    @Test
    public void aggiungiFoto() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma);
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new Date(), "ciao", "mr");

        contributor.addPuntoInteresse(new AttivitaFactory(LocalDate.now()).creaPoi("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015)));
        PuntoInteresse puntoInteresse = comune.getContenutoController().getContenuti().getFirst();

        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new Date(), "eroe", "AN2");
        TuristaLoggato turistaLoggato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();

        assertEquals(0, puntoInteresse.getMaterialeList().size());
        turistaLoggato.addPhoto(puntoInteresse, new Foto(turistaLoggato));
        assertEquals(1, puntoInteresse.getMaterialeList().size());
        assertFalse(puntoInteresse.getMaterialeList().getFirst().isApproved());
        gestorePiattaforma.promuovi(contributor, Ruolo.Curatore);
        Curatore curatore = comune.getCuratori().getFirst();
        curatore.valuta(puntoInteresse.getMaterialeList().getFirst(), true);
        assertTrue(puntoInteresse.getMaterialeList().getFirst().isApproved());
    }
}