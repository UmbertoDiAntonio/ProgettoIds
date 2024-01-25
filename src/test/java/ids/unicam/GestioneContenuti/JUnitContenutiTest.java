package ids.unicam.GestioneContenuti;

import ids.unicam.controller.ContenutoController;
import ids.unicam.controller.ContestController;
import ids.unicam.models.Comune;
import ids.unicam.models.Gradi;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.POIFactory.AttivitaFactory;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import org.junit.Test;

import java.time.LocalDate;
import java.util.Date;

import static org.junit.Assert.*;

public class JUnitContenutiTest {
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
        Comune comune = new Comune("Milano", gestorePiattaforma, new ContenutoController(), new ContestController());

        {
            gestorePiattaforma.getGestoreController().registraContributor(comune, "Mario", "Rossi", new Date(), "pass", "user");
            Contributor contributor = comune.getContributors().getFirst();

            assertEquals(0, comune.getContenutoController().getContenuti().size());
            AttivitaFactory attivita1 = new AttivitaFactory(LocalDate.now());
            PuntoInteresse punto1 = attivita1.creaPoi("bar", new Punto(comune.getPosizione().getLatitudine() + 0.01, comune.getPosizione().getLongitudine() + 0.01));
            contributor.addPuntoInteresse(punto1);
            assertEquals(1, comune.getContenutoController().getContenuti().size());
            assertFalse(comune.getContenutoController().getContenuti().getFirst().isApproved());
        }

        {
            gestorePiattaforma.getGestoreController().registraContributor(comune, "Mario", "Rossi", new Date(), "pass", "user");
            gestorePiattaforma.promuovi(comune, comune.getContributors().getFirst(), Gradi.ContributorTrusted);

            ContributorTrusted contributorTrusted = comune.getContributorTrusteds().getFirst();
            assertEquals(1, comune.getContenutoController().getContenuti().size());
            AttivitaFactory attivita1 = new AttivitaFactory(LocalDate.now());
            PuntoInteresse punto2 = attivita1.creaPoi("bar2", new Punto(comune.getPosizione().getLatitudine() + 0.02, comune.getPosizione().getLongitudine() + 0.02));
            contributorTrusted.addPuntoInteresse(punto2);
            assertEquals(2, comune.getContenutoController().getContenuti().size());
            assertTrue(comune.getContenutoController().getContenuti().getLast().isApproved());
        }

        {
            gestorePiattaforma.getGestoreController().registraContributor(comune, "Mario", "Rossi", new Date(), "pass", "user");
            gestorePiattaforma.promuovi(comune, comune.getContributors().getFirst(), Gradi.ContributorTrusted);

            ContributorTrusted contributorTrusted = comune.getContributorTrusteds().getFirst();
            AttivitaFactory attivita1 = new AttivitaFactory(LocalDate.now());
            PuntoInteresse punto3 = attivita1.creaPoi("bar3", new Punto(comune.getPosizione().getLatitudine() + 2, comune.getPosizione().getLongitudine() + 2));
            assertFalse(contributorTrusted.addPuntoInteresse(punto3));

        }

    }

    @Test
    public void creaItinerario() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma, new ContenutoController(), new ContestController());

        {
            gestorePiattaforma.getGestoreController().registraContributor(comune, "Mario", "Rossi", new Date(), "pass", "user");
            gestorePiattaforma.promuovi(comune, comune.getContributors().getFirst(), Gradi.ContributorTrusted);

            ContributorTrusted contributorTrusted = comune.getContributorTrusteds().getFirst();
            AttivitaFactory attivita1 = new AttivitaFactory(LocalDate.now());
            contributorTrusted.addPuntoInteresse(attivita1.creaPoi("bar2", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03)));
            contributorTrusted.addPuntoInteresse(attivita1.creaPoi("barcentrale", new Punto(comune.getPosizione().getLatitudine() - 0.02, comune.getPosizione().getLongitudine() - 0.02)));
            Itinerario itenerario1 = contributorTrusted.creaItinerario("girodeibar", comune.getContenutoController().getContenuti().get(0)
                    , comune.getContenutoController().getContenuti().get(1));
            assertEquals(1, comune.getContenutoController().getItinerari().size());
            assertEquals(2, comune.getContenutoController().getItinerari().values().stream().toList().getFirst().getNumeroTappe());

            PuntoInteresse nuovoPunto = attivita1.creaPoi("birreria", new Punto(comune.getPosizione().getLatitudine() + 0.14, comune.getPosizione().getLongitudine() + 0.14));
            contributorTrusted.addPuntoInteresse(nuovoPunto);
            itenerario1.addTappa(nuovoPunto);
            assertEquals(1, comune.getContenutoController().getItinerari().size());
            assertEquals(3, comune.getContenutoController().getItinerari().values().stream().toList().getFirst().getNumeroTappe());
        }
    }

    @Test
    public void creaContest() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma, new ContenutoController(), new ContestController());

        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new Date(), "ciao", "mr");
        gestorePiattaforma.promuovi(comune, contributor, Gradi.Animatore);
        Animatore animatore = comune.getAnimatori().getFirst();
        animatore.creaContest("Monumento", "Foto più bella", true);
        assertEquals(1, comune.getContestController().getContestByAuthor(Long.parseLong(comune.getAnimatori().getFirst().getId())).size());

        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new Date(), "eroe", "AN2");
        TuristaLoggato turistaLoggato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();
        turistaLoggato.joinFreeContest(comune.getContestController().getContestByAuthor(Long.parseLong(comune.getAnimatori().getFirst().getId())).getFirst());
        assertEquals(1, comune.getContestController().getContestByAuthor(Long.parseLong(comune.getAnimatori().getFirst().getId())).getFirst().getPartecipanti().size());

    }

    @Test
    public void creaContestSuInvito() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma, new ContenutoController(), new ContestController());

        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new Date(), "ciao", "mr");
        gestorePiattaforma.promuovi(comune, contributor, Gradi.Animatore);
        Animatore animatore = comune.getAnimatori().getFirst();
        animatore.creaContest("monumento", "Foto più bella", false);
        assertEquals(1, comune.getContestController().getContestByAuthor(Long.parseLong(comune.getAnimatori().getFirst().getId())).size());

        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new Date(), "eroe", "AN2");
        TuristaLoggato turistaLoggato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();
        animatore.invita(comune.getContestController().getContests().getFirst(), turistaLoggato);
        turistaLoggato.accettaInvito(turistaLoggato.getInvitiRicevuti().getFirst());
        assertEquals(1, comune.getContestController().getContests().getFirst().getPartecipanti().size());
        assertEquals(comune.getContestController().getContests().getFirst(), comune.getContestController().getContestByTurist(turistaLoggato.getId()).getFirst());

    }

    @Test
    public void aggiuntaMateriale() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma, new ContenutoController(), new ContestController());
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new Date(), "ciao", "mr");
        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new Date(), "eroe", "AN2");
        TuristaLoggato turistaLoggato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();

        {
            gestorePiattaforma.promuovi(comune, contributor, Gradi.Animatore);
            Animatore animatore = comune.getAnimatori().getFirst();
            animatore.creaContest("monumento", "Foto più bella", true);
            Materiale materiale = new Materiale(true, turistaLoggato);
            turistaLoggato.joinFreeContest(comune.getContestController().getContests().getFirst());
            turistaLoggato.addMaterialeContest(comune.getContestController().getContests().getFirst(), materiale);
            assertEquals(1, comune.getContestController().getContests().getFirst().getMaterialiContest().size());
        }

        {
            AttivitaFactory attivitaFactory = new AttivitaFactory(LocalDate.now());
            PuntoInteresse puntoInteresse = attivitaFactory.creaPoi("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));
            Materiale materiale = new Materiale(true, turistaLoggato);
            contributor.addMateriale(puntoInteresse, materiale);
            assertEquals(1, puntoInteresse.getMaterialeList().size());
        }

    }

    @Test
    public void approvaMaterialeByAnimatore(){
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma, new ContenutoController(), new ContestController());
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new Date(), "ciao", "mr");
        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new Date(), "eroe", "AN2");
        TuristaLoggato turistaLoggato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();

        gestorePiattaforma.promuovi(comune,contributor,Gradi.Animatore);
        Animatore animatore = comune.getAnimatori().getFirst();
        Contest contest = animatore.creaContest("monumento", "Foto più bella", true);
        Materiale materiale = new Materiale(true, turistaLoggato);
        turistaLoggato.addMaterialeContest(contest, materiale);
        assertFalse(materiale.isApproved());
        assertEquals(1, contest.getMaterialiContest().size());

        animatore.approva(materiale);
        assertTrue(materiale.isApproved());


    }

}