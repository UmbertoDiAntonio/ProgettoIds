package ids.unicam.GestioneContenuti;

import ids.unicam.Comune;
import ids.unicam.Exception.ContestException;
import ids.unicam.models.Orario;
import ids.unicam.models.Ruolo;
import ids.unicam.models.Tempo;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.*;
import ids.unicam.models.contenuti.POIFactory.AttivitaFactory;
import ids.unicam.models.contenuti.POIFactory.MuseoFactory;
import ids.unicam.utilites.DayOfWeek;
import ids.unicam.utilites.Punto;
import ids.unicam.utilites.Stato;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.time.LocalTime;
import java.util.GregorianCalendar;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
public class JUnitContenutiTest {

    private final GestorePiattaforma gestorePiattaforma;

    @Autowired
    public JUnitContenutiTest(GestorePiattaforma gestorePiattaforma) {
        this.gestorePiattaforma = gestorePiattaforma;
    }

    /*
     * Test relativo al corretto passaggio delle coordinate
     */
    @Test
    @Order(0)
    public void testCoordinate() {
        {
            Punto first = new Punto(0, 0);
            Punto second = new Punto(10, 0);
            assertEquals(10, first.getDistanza(second), 0);
            assertEquals(100, first.getDistanzaAlQuadrato(second), 0);
        }
    }

    /*
     * Test per creare un punto di interesse
     */
    @Test
    @Order(1)
    public void testPoi() {
        Comune comune = new Comune("Milano", gestorePiattaforma);
        gestorePiattaforma.getGestoreController().registraContributor(comune, "Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "pass", "user");
        Contributor contributor = comune.getContributors().getFirst();

        /*
         * Creazione di un Punto di Interesse da parte di un contributor, che di base non è approvato
         */
        {

            assertEquals(0, comune.getContenutoController().getContenuti().size());
            AttivitaFactory attivita1 = new AttivitaFactory();
            PuntoInteresse punto1 = attivita1.creaPoi("bar", new Punto(comune.getPosizione().getLatitudine() + 0.01, comune.getPosizione().getLongitudine() + 0.01));
            contributor.aggiungiPuntoInteresse(punto1);
            assertEquals(1, comune.getContenutoController().getContenuti().size());
            assertFalse(comune.getContenutoController().getContenuti().getFirst().getStato().asBoolean());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted, che di base è approvato
         */
        {
            gestorePiattaforma.promuovi(comune.getContributors().getFirst(), Ruolo.ContributorTrusted);

            ContributorAutorizzato contributorAutorizzato = comune.getContributorAutorizzati().getFirst();
            assertEquals(1, comune.getContenutoController().getContenuti().size());
            AttivitaFactory attivita1 = new AttivitaFactory();
            PuntoInteresse punto2 = attivita1.creaPoi("bar2", new Punto(comune.getPosizione().getLatitudine() + 0.02, comune.getPosizione().getLongitudine() + 0.02));
            contributorAutorizzato.aggiungiPuntoInteresse(punto2);
            assertEquals(2, comune.getContenutoController().getContenuti().size());
            assertTrue(comune.getContenutoController().getContenuti().getLast().getStato().asBoolean());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted, che di base è approvato, ma fuori dall'area del comune
         * quindi non può essere creato
         */
        {
            ContributorAutorizzato contributorAutorizzato = comune.getContributorAutorizzati().getFirst();
            AttivitaFactory attivita1 = new AttivitaFactory();
            PuntoInteresse punto3 = attivita1.creaPoi("bar3", new Punto(comune.getPosizione().getLatitudine() + 2, comune.getPosizione().getLongitudine() + 2));
            assertFalse(contributorAutorizzato.aggiungiPuntoInteresse(punto3));

        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted
         * aggiunta di un materiale al punto di interesse
         * verifica finale
         */
        {
            ContributorAutorizzato contributorAutorizzato = comune.getContributorAutorizzati().getFirst();
            AttivitaFactory attivita1 = new AttivitaFactory();
            PuntoInteresse puntoInteresse = attivita1.creaPoi("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));
            contributorAutorizzato.aggiungiPuntoInteresse(puntoInteresse);
            gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.FEBRUARY, 3), "eroe", "AN2");
            TuristaAutenticato turistaAutenticato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();
            MaterialeGenerico materialeGenerico = new Foto(turistaAutenticato);
            contributorAutorizzato.aggiungiMateriale(puntoInteresse, materialeGenerico);
            assertEquals(1, puntoInteresse.getMateriali().size());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted
         * aggiunta di un materiale al punto di interesse da parte di un contributor
         * approvazione da parte del curatore
         * sottoscrizione observer per la notifica
         * verifica finale
         */
        {
            gestorePiattaforma.getGestoreController().registraContributor(comune, "Peppe", "Peppe", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "PASS", "user");
            gestorePiattaforma.promuovi(comune.getContributors().getFirst(), Ruolo.Curatore);
            Curatore curatore = comune.getCuratori().getFirst();

            Orario orarioAccademia = new Orario();
            orarioAccademia.setOrarioApertura(DayOfWeek.MONDAY, LocalTime.of(9, 0), LocalTime.of(18, 0));
            contributor.aggiungiPuntoInteresse(new MuseoFactory().creaPoi("Accademia", new Punto(comune.getPosizione().getLatitudine() + 0.01, comune.getPosizione().getLongitudine() + 0.01), orarioAccademia));
            assertFalse(comune.getContenutoController().getContenuti().getLast().getStato().asBoolean());
            curatore.aggiungiOsservatore(contributor);
            assertEquals(1, curatore.getOsservatori().size());
            curatore.valuta(comune.getContenutoController().getContenuti().getLast(), Stato.APPROVED);
            assertTrue(comune.getContenutoController().getContenuti().getLast().getStato().asBoolean());
            MaterialeGenerico materialeGenerico1 = new Foto(contributor);
            comune.getContenutoController().getContenuti().getLast().getMateriali().add(materialeGenerico1);
            assertFalse(comune.getContenutoController().getContenuti().getLast().getMateriali().getFirst().getStato().asBoolean());
            curatore.valuta(comune.getContenutoController().getContenuti().getLast().getMateriali().getFirst(), Stato.APPROVED);
            assertTrue(comune.getContenutoController().getContenuti().getLast().getMateriali().getFirst().getStato().asBoolean());
            curatore.rimuoviOsservatore(contributor);
            assertEquals(0, curatore.getOsservatori().size());
        }

    }

    /*
     * Test per le varie possibilità di gestione di un itinerario
     */
    @Test
    @Order(2)
    public void testItinerario() {

        Comune comune = new Comune("Milano", gestorePiattaforma);

        /*
         * Creazione di un itenerario, aggiunta di una tappa e test.
         * Aggiunti di ulteriori tappe e test
         */

        {
            gestorePiattaforma.getGestoreController().registraContributor(comune, "Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "pass", "user");
            gestorePiattaforma.promuovi(comune.getContributors().getFirst(), Ruolo.ContributorTrusted);

            ContributorAutorizzato contributorAutorizzato = comune.getContributorAutorizzati().getFirst();
            AttivitaFactory attivita1 = new AttivitaFactory();
            contributorAutorizzato.aggiungiPuntoInteresse(attivita1.creaPoi("bar2", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03)));
            contributorAutorizzato.aggiungiPuntoInteresse(attivita1.creaPoi("barcentrale", new Punto(comune.getPosizione().getLatitudine() - 0.02, comune.getPosizione().getLongitudine() - 0.02)));
            Itinerario itinerario1 = contributorAutorizzato.creaItinerario("girodeibar", comune.getContenutoController().getContenuti().get(0), comune.getContenutoController().getContenuti().get(1));
            assertEquals(1, comune.getContenutoController().getItinerari().size());
            assertEquals(2, comune.getContenutoController().getItinerari().getFirst().getNumeroTappe());

            PuntoInteresse nuovoPunto = attivita1.creaPoi("birreria", new Punto(comune.getPosizione().getLatitudine() + 0.14, comune.getPosizione().getLongitudine() + 0.14));
            contributorAutorizzato.aggiungiPuntoInteresse(nuovoPunto);
            assert itinerario1 != null;
            comune.getContenutoController().aggiungiTappa(itinerario1, nuovoPunto);
            assertEquals(1, comune.getContenutoController().getItinerari().size());
            assertEquals(3, comune.getContenutoController().getItinerari().getFirst().getNumeroTappe());
            comune.getContenutoController().aggiungiTappa(itinerario1, nuovoPunto, nuovoPunto, nuovoPunto);
            assertEquals(1, comune.getContenutoController().getItinerari().size());
            assertEquals(6, comune.getContenutoController().getItinerari().getFirst().getNumeroTappe());
        }
    }

    /*
     * Test relativo alla gestione dei Contest
     */

    @Test
    @Order(3)
    public void testContest() {

        Comune comune = new Comune("Milano", gestorePiattaforma);
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.APRIL, 7), "ciao", "mr");
        gestorePiattaforma.promuovi(contributor, Ruolo.Animatore);
        Animatore animatore = comune.getAnimatori().getFirst();


        /*
         * Creazione di un nuovo contesto libero e successivo join del turista al contest
         * aggiunta di un materiale al contest dal turistaLoggato
         */

        {
            animatore.creaContest("Monumento", "Foto più bella", true);
            assertEquals(1, comune.getContestController().getContestDaAutore((comune.getAnimatori().getFirst().getId())).size());

            gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "eroe", "AN2");
            TuristaAutenticato turistaAutenticato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();
            MaterialeGenerico materialeGenerico = new Foto(turistaAutenticato);
            turistaAutenticato.partecipaAlContest(comune.getContestController().getContestDaAutore((comune.getAnimatori().getFirst().getId())).getFirst());
            assertEquals(1, comune.getContestController().getContestDaAutore((comune.getAnimatori().getFirst().getId())).getFirst().getPartecipanti().size());
            turistaAutenticato.aggiungiMaterialeAlContest(comune.getContestController().getContests().getFirst(), materialeGenerico);
            assertEquals(1, comune.getContestController().getContests().getFirst().getMateriali().size());
        }

        /*
         * Creazione di un contest privato, quindi su invito.
         * Invio dell'invito da parte dell'animatore
         * Verifica della validità dell'invito
         * Accettazione invito da parte del turista
         */

        {
            animatore.creaContest("monumento", "Foto più bella", false);
            assertEquals(2, comune.getContestController().getContestDaAutore((comune.getAnimatori().getFirst().getId())).size());

            gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.NOVEMBER, 5), "eroe", "AN2");
            TuristaAutenticato turistaAutenticato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();
            animatore.invita(comune.getContestController().getContests().getLast(), turistaAutenticato);

            assertTrue(turistaAutenticato.getInvitiRicevuti().getFirst().isValid());
            assertTrue(turistaAutenticato.accettaInvito(turistaAutenticato.getInvitiRicevuti().getFirst()));
            assertEquals(1, comune.getContestController().getContests().getLast().getPartecipanti().size());
            assertEquals(comune.getContestController().getContests().getLast(), comune.getContestController().getContestDelTurista(turistaAutenticato.getId()).getLast());
        }

    }

    /*
     * Test per approvare un materiale inserito da un turista su un contest
     * Inizialmente quindi non iscritto al contest
     * Successivamente entra nel contest (aperto) e carica un materiale non approvato
     * l'animatore approva la richiesta di aggiunta materiale
     */
    @Test
    @Order(4)
    public void approvaMaterialeByAnimatore() {

        Comune comune = new Comune("Milano", gestorePiattaforma);
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.OCTOBER, 1), "ciao", "mr");
        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.DECEMBER, 3), "eroe", "AN2");
        TuristaAutenticato turistaAutenticato = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getFirst();

        gestorePiattaforma.promuovi(contributor, Ruolo.Animatore);
        Animatore animatore = comune.getAnimatori().getFirst();
        Contest contest = animatore.creaContest("monumento", "Foto più bella", true);
        MaterialeGenerico materialeGenerico = new Descrizione(turistaAutenticato);
        assertThrows(ContestException.class, () -> turistaAutenticato.aggiungiMaterialeAlContest(contest, materialeGenerico));
        turistaAutenticato.partecipaAlContest(contest);
        turistaAutenticato.aggiungiMaterialeAlContest(contest, materialeGenerico);
        assertFalse(materialeGenerico.getStato().asBoolean());
        assertEquals(1, contest.getMateriali().size());

        animatore.approva(materialeGenerico);
        assertTrue(materialeGenerico.getStato().asBoolean());

    }

    /*
     * Test relativo all'eliminazione di un contenuto, che può essere un POI, un materiale, un itinerario e un contest
     */
    @Test
    @Order(5)
    public void eliminaContenuto() {

        Comune comune = new Comune("Milano", gestorePiattaforma);
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 5), "ciao", "mr");
        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "eroe", "AN2");
        Contributor contributor2 = gestorePiattaforma.getGestoreController().registraContributor(comune, "Leonardo", "rosso", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "esc", "org");
        Contributor contributor3 = gestorePiattaforma.getGestoreController().registraContributor(comune, "Fede", "Verde", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "arg", "use");

        gestorePiattaforma.promuovi(contributor2, Ruolo.Curatore);
        gestorePiattaforma.promuovi(contributor3, Ruolo.Animatore);
        Animatore animatore = comune.getAnimatori().getFirst();
        Curatore curatore = comune.getCuratori().getFirst();

        gestorePiattaforma.getGestoreController().registraTurista("aldo", "neri", new GregorianCalendar(2002, 7, 12), "password", "user1234");
        TuristaAutenticato turista = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getLast();


        AttivitaFactory attivita1 = new AttivitaFactory();
        contributor.aggiungiPuntoInteresse(attivita1.creaPoi("bar2", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03)));

        assertEquals(1, comune.getContenutoController().getContenuti().size());
        curatore.valuta(comune.getContenutoController().getContenuti().getLast(), Stato.APPROVED);
        turista.aggiungiPreferito(comune.getContenutoController().getContenuti().getLast());
        assertEquals(1, turista.getPreferiti().size());
        curatore.elimina(comune.getContenutoController().getContenuti().getLast());

        assertEquals(0, comune.getContenutoController().getContenuti().size());
        //assertEquals(0, turista.getPreferiti().size());//TODO

        contributor.aggiungiPuntoInteresse(attivita1.creaPoi("bar2", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03)));


        contributor.creaItinerario("girodeibar", comune.getContenutoController().getContenuti().getFirst());
        assertEquals(0, comune.getContenutoController().getItinerari().size());
        curatore.valuta(comune.getContenutoController().getContenuti().getFirst(), Stato.toStatus(true));
        Itinerario itinerario2 = contributor.creaItinerario("giro dei bar", comune.getContenutoController().getContenuti().getFirst());
        assertEquals(1, comune.getContenutoController().getItinerari().size());
        curatore.elimina(itinerario2);
        assertEquals(0, comune.getContenutoController().getItinerari().size());

        animatore.creaContest("contest", "spiaggia", true);
        assertEquals(1, comune.getContestController().getContests().size());
        curatore.elimina(comune.getContestController().getContests().getFirst());
        assertEquals(0, comune.getContestController().getContests().size());

        Itinerario itinerario3 = contributor.creaItinerario("girodeibar2", comune.getContenutoController().getContenuti().getFirst());
        assertEquals(1, comune.getContenutoController().getItinerari().getFirst().getNumeroTappe());
        assertTrue(contributor.aggiungiTappaItinerario(itinerario3, comune.getContenutoController().getContenuti().getFirst()));
        assertEquals(2, comune.getContenutoController().getItinerari().getFirst().getNumeroTappe());
        curatore.rimuoviTappa(itinerario3, comune.getContenutoController().getContenuti().getFirst());
        assertEquals(1, comune.getContenutoController().getItinerari().getFirst().getNumeroTappe());

        Foto foto = new Foto(turista);

        contributor.aggiungiPuntoInteresse(attivita1.creaPoi("Ristorante Stellato", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03)));
        comune.getContenutoController().getContenuti().getLast().getMateriali().add(foto);
        curatore.valuta(foto, Stato.toStatus(true));
        assertEquals(1, comune.getContenutoController().getContenuti().getLast().getMateriali().size());
        curatore.elimina(foto);
        assertEquals(0, comune.getContenutoController().getContenuti().getLast().getMateriali().size());


        //TODO aggiungere eliminazione dai preferiti
    }

    /*
     * Test per modificare la scadenza di un contentuto
     */

    @Test
    @Order(6)
    public void modificaScadenzaContenuto() {

        Comune comune = new Comune("Milano", gestorePiattaforma);
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "ciao", "mr");

        AttivitaFactory attivita1 = new AttivitaFactory();
        PuntoInteresse puntoInteresse = attivita1.creaPoi("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));

        contributor.aggiungiPuntoInteresse(puntoInteresse);

        assertThrows(UnsupportedOperationException.class, () -> contributor.aggiungiScadenzaContenuto(puntoInteresse, new Tempo()));
        //TODO test modifica scadenza
    }
}