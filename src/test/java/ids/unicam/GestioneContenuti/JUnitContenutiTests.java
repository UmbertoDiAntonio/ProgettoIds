package ids.unicam.GestioneContenuti;

import ids.unicam.Comune;
import ids.unicam.controller.ComuneController;
import ids.unicam.exception.ContestException;
import ids.unicam.models.Orario;
import ids.unicam.models.Ruolo;
import ids.unicam.models.Service.*;
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
public class JUnitContenutiTests {

    private final GestorePiattaforma gestorePiattaforma;
    private final ComuneController comuneController;
    private final GestioneComuneService gestioneComuneService;
    private final ContributorService contributorService;
    private final ContributorAutorizzatoService contributorAutorizzatoService;
    private final CuratoreService curatoreService;
    private final AnimatoreService animatoreService;
    private final PoiService poiService;
    private final ItinerarioService itinerarioService;
    private final MaterialeService materialeService;

    @Autowired
    public JUnitContenutiTests(GestorePiattaforma gestorePiattaforma, ComuneController comuneController, GestioneComuneService gestioneComuneService, ContributorService contributorService, ContributorAutorizzatoService contributorAutorizzatoService, CuratoreService curatoreService, AnimatoreService animatoreService, PoiService poiService, ItinerarioService itinerarioService, MaterialeService materialeService) {
        this.gestorePiattaforma = gestorePiattaforma;
        this.comuneController = comuneController;
        this.gestioneComuneService = gestioneComuneService;
        this.contributorService = contributorService;
        this.contributorAutorizzatoService = contributorAutorizzatoService;
        this.curatoreService = curatoreService;
        this.animatoreService = animatoreService;
        this.poiService = poiService;
        this.itinerarioService = itinerarioService;
        this.materialeService = materialeService;
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
        Comune comune = comuneController.creaComune("Milano", gestorePiattaforma);
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "pass", "user");

        /*
         * Creazione di un Punto di Interesse da parte di un contributor, che di base non è approvato
         */
        {

            assertEquals(0, gestioneComuneService.getContenuti(comune.getNome()).size());
            AttivitaFactory attivita1 = new AttivitaFactory();
            PuntoInteresse punto1 = attivita1.creaPoi(comune,"bar", new Punto(comune.getPosizione().getLatitudine() + 0.01, comune.getPosizione().getLongitudine() + 0.01));
            contributorService.aggiungiPuntoInteresse(contributor,punto1);//TODO check se si mette
            

            assertEquals(1, gestioneComuneService.getContenuti(comune.getNome()).size());
            assertFalse(gestioneComuneService.getContenuti(comune.getNome()).getFirst().getStato().asBoolean());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted, che di base è approvato
         */
        {
            gestorePiattaforma.promuovi(gestioneComuneService.getContributorDelComune(comune.getNome()).getLast(), Ruolo.ContributorTrusted);

            ContributorAutorizzato contributorAutorizzato = gestioneComuneService.getContributorAutorizzatiDelComune(comune.getNome()).getLast();

            assertEquals(1, gestioneComuneService.getContenuti(comune.getNome()).size());
            AttivitaFactory attivita1 = new AttivitaFactory();
            PuntoInteresse punto2 = attivita1.creaPoi(comune,"bar2", new Punto(comune.getPosizione().getLatitudine() + 0.02, comune.getPosizione().getLongitudine() + 0.02));

            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato,punto2);//TODO check e true
            //il problema qui è avere 2 istanze diverse del Controller
            assertEquals(2, gestioneComuneService.getContenuti(comune.getNome()).size());
            assertTrue(gestioneComuneService.getContenuti(comune.getNome()).getLast().getStato().asBoolean());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted, che di base è approvato, ma fuori dall'area del comune
         * quindi non può essere creato
         */
        {
            ContributorAutorizzato contributorAutorizzato = gestioneComuneService.getContributorAutorizzatiDelComune(comune.getNome()).getFirst();
            AttivitaFactory attivita1 = new AttivitaFactory();
            PuntoInteresse punto3 = attivita1.creaPoi(comune,"bar3", new Punto(comune.getPosizione().getLatitudine() + 2, comune.getPosizione().getLongitudine() + 2));
            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato,punto3);//TODO check se true

        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted
         * aggiunta di un materiale al punto di interesse
         * verifica finale
         */
        {
            ContributorAutorizzato contributorAutorizzato = gestioneComuneService.getContributorAutorizzatiDelComune(comune.getNome()).getFirst();
            AttivitaFactory attivita1 = new AttivitaFactory();
            PuntoInteresse puntoInteresse = attivita1.creaPoi(comune,"Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));
            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato,puntoInteresse);
            gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.FEBRUARY, 3), "eroe", "AN2");
            TuristaAutenticato turistaAutenticato = gestorePiattaforma.getGestoreController().prendiPrimoTurista();
            MaterialeGenerico materialeGenerico = new Foto(turistaAutenticato);
            contributorAutorizzatoService.aggiungiMateriale(contributorAutorizzato, puntoInteresse, materialeGenerico);
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
            gestorePiattaforma.promuovi(gestioneComuneService.getContributorDelComune(comune.getNome()).getFirst(), Ruolo.Curatore);
            Curatore curatore = gestioneComuneService.getCuratoriDelComune(comune.getNome()).getFirst();

            Orario orarioAccademia = new Orario();
            orarioAccademia.setOrarioApertura(DayOfWeek.MONDAY, LocalTime.of(9, 0), LocalTime.of(18, 0));
            contributorService.aggiungiPuntoInteresse(contributor,new MuseoFactory().creaPoi(comune,"Accademia", new Punto(comune.getPosizione().getLatitudine() + 0.01, comune.getPosizione().getLongitudine() + 0.01), orarioAccademia));
            assertFalse(gestioneComuneService.getContenuti(comune.getNome()).getLast().getStato().asBoolean());
            curatore.aggiungiOsservatore(contributor);
            assertEquals(1, curatore.getOsservatori().size());
            
            curatoreService.valuta(gestioneComuneService.getContenuti(comune.getNome()).getLast(), Stato.APPROVED);
            assertTrue(gestioneComuneService.getContenuti(comune.getNome()).getLast().getStato().asBoolean());
            MaterialeGenerico materialeGenerico1 = new Foto(contributor);
            gestioneComuneService.getContenuti(comune.getNome()).getLast().getMateriali().add(materialeGenerico1);
            assertFalse(gestioneComuneService.getContenuti(comune.getNome()).getLast().getMateriali().getFirst().getStato().asBoolean());
            curatoreService.valuta(gestioneComuneService.getContenuti(comune.getNome()).getLast().getMateriali().getFirst(), Stato.APPROVED);
            assertTrue(gestioneComuneService.getContenuti(comune.getNome()).getLast().getMateriali().getFirst().getStato().asBoolean());
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

        Comune comune = comuneController.creaComune("Milano", gestorePiattaforma);

        /*
         * Creazione di un itenerario, aggiunta di una tappa e test.
         * Aggiunti di ulteriori tappe e test
         */

        {
            gestorePiattaforma.getGestoreController().registraContributor(comune, "Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "pass", "user");
            gestorePiattaforma.promuovi(gestioneComuneService.getContributorDelComune(comune.getNome()).getFirst(), Ruolo.ContributorTrusted);

            ContributorAutorizzato contributorAutorizzato = gestioneComuneService.getContributorAutorizzatiDelComune(comune.getNome()).getFirst();
            AttivitaFactory attivita1 = new AttivitaFactory();
            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato,attivita1.creaPoi(comune,"bar2", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03)));
            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato,attivita1.creaPoi(comune,"barcentrale", new Punto(comune.getPosizione().getLatitudine() - 0.02, comune.getPosizione().getLongitudine() - 0.02)));
            Itinerario itinerario1 = contributorAutorizzatoService.aggiungiItinerario(new Itinerario(comune,"girodeibar", gestioneComuneService.getContenuti(comune.getNome()).get(0), gestioneComuneService.getContenuti(comune.getNome()).get(1)));
            
            assertEquals(1, itinerarioService.findAllByComune(comune).size());
            assertEquals(2, itinerarioService.findAllByComune(comune).getFirst().getNumeroTappe());

            PuntoInteresse nuovoPunto = attivita1.creaPoi(comune,"birreria", new Punto(comune.getPosizione().getLatitudine() + 0.14, comune.getPosizione().getLongitudine() + 0.14));
            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato,nuovoPunto);
            assert itinerario1 != null;
            
            itinerarioService.aggiungiTappa(itinerario1, nuovoPunto);
            assertEquals(1, itinerarioService.findAllByComune(comune).size());
            assertEquals(3, itinerarioService.findAllByComune(comune).getFirst().getNumeroTappe());
            itinerarioService.aggiungiTappa(itinerario1, nuovoPunto, nuovoPunto, nuovoPunto);
            assertEquals(1, itinerarioService.findAllByComune(comune).size());
            assertEquals(6, itinerarioService.findAllByComune(comune).getFirst().getNumeroTappe());
        }
    }

    /*
     * Test relativo alla gestione dei Contest
     */

    @Test
    @Order(3)
    public void testContest() {

        Comune comune = comuneController.creaComune("Milano", gestorePiattaforma);
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.APRIL, 7), "ciao", "mr");
        gestorePiattaforma.promuovi(contributor, Ruolo.Animatore);
        Animatore animatore = gestioneComuneService.getAnimatoriDelComune(comune.getNome()).getFirst();


        /*
         * Creazione di un nuovo contesto libero e successivo join del turista al contest
         * aggiunta di un materiale al contest dal turistaLoggato
         */

        {
            animatore.creaContest("Monumento", "Foto più bella", true);
            assertEquals(1, comune.getContestController().getContestDaAutore((gestioneComuneService.getAnimatoriDelComune(comune.getNome()).getFirst().getId())).size());

            gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "eroe", "AN2");
            TuristaAutenticato turistaAutenticato = gestorePiattaforma.getGestoreController().prendiPrimoTurista();
            MaterialeGenerico materialeGenerico = new Foto(turistaAutenticato);
            turistaAutenticato.partecipaAlContest(comune.getContestController().getContestDaAutore((gestioneComuneService.getAnimatoriDelComune(comune.getNome()).getFirst().getId())).getFirst());
            assertEquals(1, comune.getContestController().getContestDaAutore((gestioneComuneService.getAnimatoriDelComune(comune.getNome()).getFirst().getId())).getFirst().getPartecipanti().size());
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
            assertEquals(2, comune.getContestController().getContestDaAutore((gestioneComuneService.getAnimatoriDelComune(comune.getNome()).getFirst().getId())).size());

            gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.NOVEMBER, 5), "eroe", "AN2");
            TuristaAutenticato turistaAutenticato = gestorePiattaforma.getGestoreController().prendiPrimoTurista();
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

        Comune comune = comuneController.creaComune("Milano", gestorePiattaforma);
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.OCTOBER, 1), "ciao", "mr");
        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.DECEMBER, 3), "eroe", "AN2");
        TuristaAutenticato turistaAutenticato = gestorePiattaforma.getGestoreController().prendiPrimoTurista();

        gestorePiattaforma.promuovi(contributor, Ruolo.Animatore);
        Animatore animatore = gestioneComuneService.getAnimatoriDelComune(comune.getNome()).getFirst();
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

        Comune comune = comuneController.creaComune("Milano", gestorePiattaforma);
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 5), "ciao", "mr");
        gestorePiattaforma.getGestoreController().registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "eroe", "AN2");
        Contributor contributor2 = gestorePiattaforma.getGestoreController().registraContributor(comune, "Leonardo", "rosso", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "esc", "org");
        Contributor contributor3 = gestorePiattaforma.getGestoreController().registraContributor(comune, "Fede", "Verde", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "arg", "use");

        gestorePiattaforma.promuovi(contributor2, Ruolo.Curatore);
        gestorePiattaforma.promuovi(contributor3, Ruolo.Animatore);
        Animatore animatore = gestioneComuneService.getAnimatoriDelComune(comune.getNome()).getFirst();
        Curatore curatore = gestioneComuneService.getCuratoriDelComune(comune.getNome()).getFirst();

        gestorePiattaforma.getGestoreController().registraTurista("aldo", "neri", new GregorianCalendar(2002, 7, 12), "password", "user1234");
        TuristaAutenticato turista = gestorePiattaforma.getGestoreController().prendiPrimoTurista();


        AttivitaFactory attivita1 = new AttivitaFactory();
        contributorService.aggiungiPuntoInteresse(contributor,attivita1.creaPoi(comune,"bar2", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03)));

        assertEquals(1, gestioneComuneService.getContenuti(comune.getNome()).size());
        curatoreService.valuta(gestioneComuneService.getContenuti(comune.getNome()).getLast(), Stato.APPROVED);
        turista.aggiungiPreferito(gestioneComuneService.getContenuti(comune.getNome()).getLast());
        assertEquals(1, turista.getPreferiti().size());
        curatoreService.eliminaContenuto(gestioneComuneService.getContenuti(comune.getNome()).getLast());

        assertEquals(0, gestioneComuneService.getContenuti(comune.getNome()).size());
        //assertEquals(0, turista.getPreferiti().size());//TODO

        contributorService.aggiungiPuntoInteresse(contributor,attivita1.creaPoi(comune,"bar2", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03)));

        
        contributorService.aggiungiItinerario(new Itinerario(comune,"girodeibar", gestioneComuneService.getContenuti(comune.getNome()).getFirst()));
        assertEquals(0, itinerarioService.findAllByComune(comune).size());
        curatoreService.valuta(gestioneComuneService.getContenuti(comune.getNome()).getFirst(), Stato.toStatus(true));
        Itinerario itinerario2 = contributorService.aggiungiItinerario(new Itinerario(comune,"giro dei bar", gestioneComuneService.getContenuti(comune.getNome()).getFirst()));
        assertEquals(1, itinerarioService.findAllByComune(comune).size());
        curatoreService.eliminaContenuto(itinerario2);
        assertEquals(0, itinerarioService.findAllByComune(comune).size());

        animatore.creaContest("contest", "spiaggia", true);
        assertEquals(1, comune.getContestController().getContests().size());
        curatoreService.eliminaContenuto(comune.getContestController().getContests().getFirst());
        assertEquals(0, comune.getContestController().getContests().size());

        Itinerario itinerario3 = contributorService.aggiungiItinerario(new Itinerario(comune,"girodeibar2", gestioneComuneService.getContenuti(comune.getNome()).getFirst()));
        assertEquals(1, itinerarioService.findAllByComune(comune).getFirst().getNumeroTappe());
        assertTrue(contributorService.aggiungiTappaItinerario(itinerario3, gestioneComuneService.getContenuti(comune.getNome()).getFirst()));
        assertEquals(2, itinerarioService.findAllByComune(comune).getFirst().getNumeroTappe());
        curatoreService.rimuoviTappa(itinerario3, gestioneComuneService.getContenuti(comune.getNome()).getFirst());
        assertEquals(1, itinerarioService.findAllByComune(comune).getFirst().getNumeroTappe());

        Foto foto = new Foto(turista);

        contributorService.aggiungiPuntoInteresse(contributor,attivita1.creaPoi(comune,"Ristorante Stellato", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03)));
        gestioneComuneService.getContenuti(comune.getNome()).getLast().getMateriali().add(foto);
        curatoreService.valuta(foto, Stato.toStatus(true));
        assertEquals(1, gestioneComuneService.getContenuti(comune.getNome()).getLast().getMateriali().size());
        curatoreService.eliminaMateriale(foto);
        assertEquals(0, gestioneComuneService.getContenuti(comune.getNome()).getLast().getMateriali().size());


        //TODO aggiungere eliminazione dai preferiti
    }

    /*
     * Test per modificare la scadenza di un contentuto
     */

    @Test
    @Order(6)
    public void modificaScadenzaContenuto() {

        Comune comune = comuneController.creaComune("Milano", gestorePiattaforma);
        Contributor contributor = gestorePiattaforma.getGestoreController().registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "ciao", "mr");

        AttivitaFactory attivita1 = new AttivitaFactory();
        PuntoInteresse puntoInteresse = attivita1.creaPoi(comune,"Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));

        contributorService.aggiungiPuntoInteresse(contributor,puntoInteresse);

        assertThrows(UnsupportedOperationException.class, () -> contributor.aggiungiScadenzaContenuto(puntoInteresse, new Tempo()));
        //TODO test modifica scadenza
    }
}