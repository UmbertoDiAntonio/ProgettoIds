package ids.unicam.GestioneContenuti;

import ids.unicam.Exception.NotInContestException;
import ids.unicam.controller.ContenutoController;
import ids.unicam.controller.ContestController;
import ids.unicam.Comune;
import ids.unicam.models.Ruolo;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.*;
import ids.unicam.models.contenuti.POIFactory.AttivitaFactory;
import ids.unicam.utilites.Punto;
import org.junit.jupiter.api.Test;


import java.time.LocalDate;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

public class JUnitContenutiTest {

    //TODO inserire commenti ad ogni TEST
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
            gestorePiattaforma.promuovi( comune.getContributors().getFirst(), Ruolo.ContributorTrusted);

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
            gestorePiattaforma.promuovi(comune.getContributors().getFirst(), Ruolo.ContributorTrusted);

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
            gestorePiattaforma.promuovi( comune.getContributors().getFirst(), Ruolo.ContributorTrusted);

            ContributorTrusted contributorTrusted = comune.getContributorTrusteds().getFirst();
            AttivitaFactory attivita1 = new AttivitaFactory(LocalDate.now());
            contributorTrusted.addPuntoInteresse(attivita1.creaPoi("bar2", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03)));
            contributorTrusted.addPuntoInteresse(attivita1.creaPoi("barcentrale", new Punto(comune.getPosizione().getLatitudine() - 0.02, comune.getPosizione().getLongitudine() - 0.02)));
            Itinerario itinerario1 = contributorTrusted.creaItinerario("girodeibar", comune.getContenutoController().getContenuti().get(0)
                    , comune.getContenutoController().getContenuti().get(1));
            assertEquals(1, comune.getContenutoController().getItinerari().size());
            assertEquals(2, comune.getContenutoController().getItinerari().getFirst().getNumeroTappe());

            PuntoInteresse nuovoPunto = attivita1.creaPoi("birreria", new Punto(comune.getPosizione().getLatitudine() + 0.14, comune.getPosizione().getLongitudine() + 0.14));
            contributorTrusted.addPuntoInteresse(nuovoPunto);
            comune.getContenutoController().addTappa(itinerario1, nuovoPunto);
            assertEquals(1, comune.getContenutoController().getItinerari().size());
            assertEquals(3, comune.getContenutoController().getItinerari().getFirst().getNumeroTappe());
        }
    }

    @Test
    public void creaContest() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma, new ContenutoController(), new ContestController());

        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new Date(), "ciao", "mr");
        gestorePiattaforma.promuovi( contributor, Ruolo.Animatore);
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
        gestorePiattaforma.promuovi( contributor, Ruolo.Animatore);
        Animatore animatore = comune.getAnimatori().getFirst();
        animatore.creaContest("monumento", "Foto più bella", false);
        assertEquals(1, comune.getContestController().getContestByAuthor(Long.parseLong(comune.getAnimatori().getFirst().getId())).size());

        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new Date(), "eroe", "AN2");
        TuristaLoggato turistaLoggato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();
        animatore.invita(comune.getContestController().getContests().getFirst(), turistaLoggato);
        turistaLoggato.accettaInvito(turistaLoggato.getInvitiRicevuti().getFirst());
        assertEquals(1, comune.getContestController().getContests().getFirst().getPartecipanti().size());
        assertEquals(comune.getContestController().getContests().getFirst(), comune.getContestController().getContestByTourist(turistaLoggato.getId()).getFirst());

    }

    @Test
    public void aggiuntaMateriale() {
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma, new ContenutoController(), new ContestController());
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new Date(), "ciao", "mr");
        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new Date(), "eroe", "AN2");
        TuristaLoggato turistaLoggato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();
        AttivitaFactory attivitaFactory = new AttivitaFactory(LocalDate.now());

        {
            gestorePiattaforma.promuovi( contributor, Ruolo.Animatore);
            Animatore animatore = comune.getAnimatori().getFirst();
            animatore.creaContest("monumento", "Foto più bella", true);
            Materiale materiale = new Foto( turistaLoggato);
            turistaLoggato.joinFreeContest(comune.getContestController().getContests().getFirst());
            turistaLoggato.addMaterialeContest(comune.getContestController().getContests().getFirst(), materiale);
            assertEquals(1, comune.getContestController().getContests().getFirst().getMaterialiContest().size());
        }

        {
            PuntoInteresse puntoInteresse = attivitaFactory.creaPoi("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));
            Materiale materiale = new Foto( turistaLoggato);

            contributor.addMateriale(puntoInteresse, materiale);
            assertEquals(1, puntoInteresse.getMaterialeList().size());
        }

        {
            PuntoInteresse puntoInteresse = attivitaFactory.creaPoi("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));
            contributor.addPuntoInteresse(puntoInteresse);
            assertEquals(0,turistaLoggato.getFavourites().size());
            turistaLoggato.addFavourites(puntoInteresse);
            assertEquals(1,turistaLoggato.getFavourites().size());

        }

    }

    @Test
    public void approvaMaterialeByAnimatore(){
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma, new ContenutoController(), new ContestController());
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new Date(), "ciao", "mr");
        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new Date(), "eroe", "AN2");
        TuristaLoggato turistaLoggato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();

        gestorePiattaforma.promuovi(contributor, Ruolo.Animatore);
        Animatore animatore = comune.getAnimatori().getFirst();
        Contest contest = animatore.creaContest("monumento", "Foto più bella", true);
        Materiale materiale = new Descrizione( turistaLoggato);
        assertThrows(NotInContestException.class, () -> turistaLoggato.addMaterialeContest(contest, materiale));
        turistaLoggato.joinFreeContest(contest);
        turistaLoggato.addMaterialeContest(contest, materiale);
        assertFalse(materiale.isApproved());
        assertEquals(1, contest.getMaterialiContest().size());

        animatore.approva(materiale);
        assertTrue(materiale.isApproved());

    }

    @Test
    public void eliminaContenuto(){
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("Milano", gestorePiattaforma, new ContenutoController(), new ContestController());
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new Date(), "ciao", "mr");
        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new Date(), "eroe", "AN2");
        Contributor contributor2 = gestorePiattaforma.getGestoreController().registraContributor(comune, "Leonardo", "rosso", new Date(), "esc", "org");
        Contributor contributor3 = gestorePiattaforma.getGestoreController().registraContributor(comune, "Fede", "Verde", new Date(), "arg", "use");

        gestorePiattaforma.promuovi(contributor2, Ruolo.Curatore);
        gestorePiattaforma.promuovi(contributor3, Ruolo.Animatore);
        Animatore animatore = comune.getAnimatori().getFirst();
        Curatore curatore = comune.getCuratori().getFirst();

        AttivitaFactory attivita1 = new AttivitaFactory(LocalDate.now());
        contributor.addPuntoInteresse(attivita1.creaPoi("bar2", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03)));

        assertEquals(1, comune.getContenutoController().getContenuti().size());
        curatore.delete(comune.getContenutoController().getContenuti().getFirst());
        assertEquals(0, comune.getContenutoController().getContenuti().size());

        contributor.addPuntoInteresse(attivita1.creaPoi("bar2", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03)));


        Itinerario itinerario1 = contributor.creaItinerario("girodeibar", comune.getContenutoController().getContenuti().getFirst());
        assertEquals(0, comune.getContenutoController().getItinerari().size());
        curatore.valuta(comune.getContenutoController().getContenuti().getFirst(), true);
        itinerario1 = contributor.creaItinerario("girodeibar", comune.getContenutoController().getContenuti().getFirst());
        assertEquals(1, comune.getContenutoController().getItinerari().size());
        curatore.delete(itinerario1);
        assertEquals(0, comune.getContenutoController().getItinerari().size());

        animatore.creaContest("contest", "spiaggia", true);
        assertEquals(1, comune.getContestController().getContests().size());
        curatore.delete(comune.getContestController().getContests().getFirst());
        assertEquals(0, comune.getContestController().getContests().size());

        itinerario1 = contributor.creaItinerario("girodeibar", comune.getContenutoController().getContenuti().getFirst());
        assertEquals(1, comune.getContenutoController().getItinerari().getFirst().getNumeroTappe());
        contributor.aggiungiTappaItinerario(itinerario1, comune.getContenutoController().getContenuti().getFirst());
        assertEquals(2, comune.getContenutoController().getItinerari().getFirst().getNumeroTappe());
        curatore.rimuoviTappa(itinerario1, comune.getContenutoController().getContenuti().getFirst());
        assertEquals(1, comune.getContenutoController().getItinerari().getFirst().getNumeroTappe());




    }

}