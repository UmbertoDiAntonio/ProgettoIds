package ids.unicam.GestioneContenuti;

import ids.unicam.controller.ContenutoController;
import ids.unicam.controller.ContestController;
import ids.unicam.controller.UtentiController;
import ids.unicam.models.Comune;
import ids.unicam.models.Gradi;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.POIFactory.AttivitaFactory;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import org.junit.Test;

import java.time.LocalDate;
import java.util.Date;

import static org.junit.Assert.assertEquals;

public class JUnitTestContenuti {
    @Test
    public void testCoordinate() {
        {
            Punto first = new Punto(0, 0);
            Punto second = new Punto(10, 0);
            assertEquals(10, first.getDistance(second), 0);
            assertEquals(100, first.getDistanceSquared(second), 0);
        }
    }


    @Test
    public void creaPoi() {

        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("nome", new Punto(1, 1), gestorePiattaforma, new ContenutoController(), new ContestController(), new UtentiController());

        {
            gestorePiattaforma.getGestoreController().registraContributor(comune, "Mario", "Rossi", new Date(), "pass", "user");
            Contributor contributor = comune.getContributors().get(0);


            assertEquals(0, comune.getContenutoController().getContenuti().size());
            AttivitaFactory attivita1 = new AttivitaFactory(LocalDate.now());
            PuntoInteresse punto1 = attivita1.creaPoi("bar", new Punto(1, 1));
            contributor.addPuntoInteresse(punto1);
            assertEquals(1, comune.getContenutoController().getContenuti().size());
            assertEquals(false, comune.getContenutoController().getContenuti().getFirst().isApproved());
        }

        {
            gestorePiattaforma.getGestoreController().registraContributor(comune, "Mario", "Rossi", new Date(), "pass", "user");
            gestorePiattaforma.promuovi(comune, comune.getContributors().get(0), Gradi.ContributorTrusted);

            ContributorTrusted contributorTrusted = comune.getContributorTrusteds().get(0);
            assertEquals(1, comune.getContenutoController().getContenuti().size());
            AttivitaFactory attivita1 = new AttivitaFactory(LocalDate.now());
            PuntoInteresse punto2 = attivita1.creaPoi("bar2", new Punto(2, 2));
            contributorTrusted.addPuntoInteresse(punto2);
            assertEquals(2, comune.getContenutoController().getContenuti().size());
            assertEquals(true, comune.getContenutoController().getContenuti().getLast().isApproved());
        }

    }

    @Test
    public void creaItinerario() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("nome", new Punto(1, 1), gestorePiattaforma, new ContenutoController(), new ContestController(), new UtentiController());

        {
            gestorePiattaforma.getGestoreController().registraContributor(comune, "Mario", "Rossi", new Date(), "pass", "user");
            gestorePiattaforma.promuovi(comune, comune.getContributors().get(0), Gradi.ContributorTrusted);

            ContributorTrusted contributorTrusted = comune.getContributorTrusteds().get(0);
            AttivitaFactory attivita1 = new AttivitaFactory(LocalDate.now());
            contributorTrusted.addPuntoInteresse(attivita1.creaPoi("bar2", new Punto(2, 2)));
            contributorTrusted.addPuntoInteresse(attivita1.creaPoi("barcentrale", new Punto(0, 0)));
            Itinerario itenerario1 = contributorTrusted.creaItinerario("girodeibar", comune.getContenutoController().getContenuti().get(0)
                    , comune.getContenutoController().getContenuti().get(1));
            assertEquals(1, comune.getContenutoController().getItinerari().size());
            assertEquals(2, comune.getContenutoController().getItinerari().values().stream().toList().getFirst().getNumeroTappe());

            PuntoInteresse nuovoPunto = attivita1.creaPoi("birreria", new Punto(4, 4));
            contributorTrusted.addPuntoInteresse(nuovoPunto);
            itenerario1.addTappa(nuovoPunto);
            assertEquals(1, comune.getContenutoController().getItinerari().size());
            assertEquals(3, comune.getContenutoController().getItinerari().values().stream().toList().getFirst().getNumeroTappe());
        }
    }

    @Test
    public void creaContest() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("nome", new Punto(1, 1), gestorePiattaforma, new ContenutoController(), new ContestController(), new UtentiController());

        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new Date(),"ciao","mr");
        gestorePiattaforma.promuovi(comune,contributor, Gradi.Animatore);
        Animatore animatore = comune.getAnimatori().getFirst();
        animatore.creaContest("Foto più bella", true);
        assertEquals(1,comune.getContestController().getContestByAuthor(Long.parseLong(comune.getAnimatori().getFirst().getId())).size());

        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new Date(), "eroe", "AN2");
        TuristaLoggato turistaLoggato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();
        turistaLoggato.joinContest(comune.getContestController().getContestByAuthor(Long.parseLong(comune.getAnimatori().getFirst().getId())).getFirst());
        assertEquals(1,comune.getContestController().getContestByAuthor(Long.parseLong(comune.getAnimatori().getFirst().getId())).getFirst().getPartecipanti().size());

    }

    @Test
    public void creaContestSuInvito(){
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("nome", new Punto(1, 1), gestorePiattaforma, new ContenutoController(), new ContestController(), new UtentiController());

        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new Date(),"ciao","mr");
        gestorePiattaforma.promuovi(comune,contributor, Gradi.Animatore);
        Animatore animatore = comune.getAnimatori().getFirst();
        animatore.creaContest("Foto più bella", false);
        assertEquals(1,comune.getContestController().getContestByAuthor(Long.parseLong(comune.getAnimatori().getFirst().getId())).size());

        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new Date(), "eroe", "AN2");
        TuristaLoggato turistaLoggato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();

    }
}


