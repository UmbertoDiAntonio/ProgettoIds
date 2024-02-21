package ids.unicam.GestioneContenuti;

import ids.unicam.DataBase.GestoreDatabase;
import ids.unicam.Service.impl.*;
import ids.unicam.exception.ContestException;
import ids.unicam.models.Comune;
import ids.unicam.models.Invito;
import ids.unicam.models.Punto;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.Descrizione;
import ids.unicam.models.contenuti.materiali.Foto;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.DayOfWeek;
import ids.unicam.models.contenuti.puntiInteresse.Orario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.TipologiaPuntoInteresse;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.GregorianCalendar;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
public class JUnitContenutiTests {
    private final ComuneServiceImpl comuneService;
    private final ContributorServiceImpl contributorService;
    private final ContributorAutorizzatoServiceImpl contributorAutorizzatoServiceImpl;
    private final CuratoreServiceImpl curatoreServiceImpl;
    private final AnimatoreServiceImpl animatoreServiceImpl;
    private final TuristaAutenticatoServiceImpl turistaAutenticatoService;
    private final PoiServiceImpl poiService;
    private final ItinerarioServiceImpl itinerarioService;
    private final ContestServiceImpl contestService;
    private final GestorePiattaformaServiceImpl gestorePiattaformaService;
    private final InvitoServiceImpl invitoService;

    @Autowired
    public JUnitContenutiTests(ComuneServiceImpl comuneService, ContributorServiceImpl contributorService, ContributorAutorizzatoServiceImpl contributorAutorizzatoServiceImpl, CuratoreServiceImpl curatoreServiceImpl, AnimatoreServiceImpl animatoreServiceImpl, TuristaAutenticatoServiceImpl turistaAutenticatoService, PoiServiceImpl poiService, ItinerarioServiceImpl itinerarioService, ContestServiceImpl contestService, GestorePiattaformaServiceImpl gestorePiattaformaService, InvitoServiceImpl invitoService, GestoreDatabase gestoreDatabase) {
        this.comuneService = comuneService;
        this.contributorService = contributorService;
        this.contributorAutorizzatoServiceImpl = contributorAutorizzatoServiceImpl;
        this.curatoreServiceImpl = curatoreServiceImpl;
        this.animatoreServiceImpl = animatoreServiceImpl;
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.poiService = poiService;
        this.itinerarioService = itinerarioService;
        this.contestService = contestService;
        this.gestorePiattaformaService = gestorePiattaformaService;
        this.invitoService = invitoService;
        gestoreDatabase.eliminaTabelleDB();
        gestoreDatabase.inizializzaDatabase();
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
        Comune comune = comuneService.creaComune("Milano");
        TuristaAutenticato turistaTemp = gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR, "Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "1Unica@", "user1");
        if (!(turistaTemp instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");
        /*
         * Creazione di un Punto di Interesse da parte di un contributor, che di base non è approvato
         */
        {
            int puntiInteresseComuneIniziali = comuneService.getPuntiInteresseNelComune(comune.getNome()).size();
            Orario orario = new Orario();
            orario.setOrarioApertura(DayOfWeek.MONDAY, LocalTime.of(8, 30), LocalTime.of(18, 0));
            PuntoInteresse punto1 = poiService.creaPuntoInteresse("bar", new Punto(comune.getPosizione().getLatitudine() + 0.01, comune.getPosizione().getLongitudine() + 0.01), orario, TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributor);

            assertEquals(puntiInteresseComuneIniziali + 1, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());
            assertFalse(punto1.getStato().asBoolean());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted, che di base è approvato
         */
        {
            gestorePiattaformaService.cambiaRuolo(contributor, Ruolo.CONTRIBUTOR_AUTORIZZATO);

            ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiDelComune(comune.getNome()).getLast();
            int puntiInteresseComuneIniziali = comuneService.getPuntiInteresseNelComune(comune.getNome()).size();

            PuntoInteresse punto2 = poiService.creaPuntoInteresse("bar2", new Punto(comune.getPosizione().getLatitudine() + 0.02, comune.getPosizione().getLongitudine() + 0.02), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributorAutorizzato);

            //il problema qui è avere 2 istanze diverse del Controller
            assertEquals(puntiInteresseComuneIniziali + 1, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());
            assertTrue(punto2.getStato().asBoolean());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted, che di base è approvato, ma fuori dall'area del comune
         * quindi non può essere creato
         */
        {
            int puntiInteresseComuneIniziali = comuneService.getPuntiInteresseNelComune(comune.getNome()).size();
            Comune comune2 = comuneService.creaComune("Roma");
            TuristaAutenticato turistaTemp2 = gestorePiattaformaService.registra(comune2, Ruolo.CONTRIBUTOR_AUTORIZZATO, "Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "1Unica@", "user19");
            if (!(turistaTemp2 instanceof ContributorAutorizzato contributorAutorizzato))
                throw new IllegalArgumentException("errore");
            assertThrows(IllegalArgumentException.class, () -> new PuntoInteresse(comune, "chiesa", new Punto(comune.getPosizione().getLatitudine() + 2, comune.getPosizione().getLongitudine() + 2), TipologiaPuntoInteresse.LUOGO_DI_CULTO, contributorAutorizzato));
            assertEquals(puntiInteresseComuneIniziali, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted
         * aggiunta di un materiale al punto di interesse
         * verifica finale
         */
        {
            ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiDelComune(comune.getNome()).getFirst();
            PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributorAutorizzato);

            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(null, Ruolo.TURISTA, "andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.FEBRUARY, 3), "2Unica@", "user2");
            MaterialeGenerico materialeGenerico = new Foto(turistaAutenticato);
            poiService.aggiungiMateriale(contributorAutorizzato, puntoInteresse, materialeGenerico);
            assertEquals(1, poiService.getMaterialiPoi(puntoInteresse).size());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted
         * aggiunta di un materiale al punto di interesse da parte di un contributor
         * approvazione da parte del curatore
         * sottoscrizione observer per la notifica
         * verifica finale
         */
        {
            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(null, Ruolo.TURISTA, "andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.FEBRUARY, 3), "3Unica@", "user3");
            TuristaAutenticato turistaTemp1 = gestorePiattaformaService.registra(comune, Ruolo.CURATORE, "Peppe", "Peppe", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "4Unica@", "user4");
            if (!(turistaTemp1 instanceof Curatore curatore))
                throw new IllegalArgumentException("errore");

            Orario orarioAccademia = new Orario();
            orarioAccademia.setOrarioApertura(DayOfWeek.MONDAY, LocalTime.of(9, 0), LocalTime.of(18, 0));

            PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse("Accademia", new Punto(comune.getPosizione().getLatitudine() + 0.01, comune.getPosizione().getLongitudine() + 0.01), orarioAccademia, TipologiaPuntoInteresse.CENTRO_SPORTIVO, contributor);

            assertFalse(puntoInteresse.getStato().asBoolean());
            TuristaAutenticato turistaTemp2 = gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR, "Peppe", "Paol", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "4Unica@", "user44");

            if (!(turistaTemp2 instanceof Contributor contributor1))
                throw new IllegalArgumentException("errore");
            TuristaAutenticato turistaTemp3 = gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR, "Pietro", "Pier", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "4Unica@", "user45");
            if (!(turistaTemp3 instanceof Contributor contributor2))
                throw new IllegalArgumentException("errore");

            int numeroOsservatori = curatoreServiceImpl.getOsservatori(curatore).size();
            curatoreServiceImpl.aggiungiOsservatore(curatore, contributor1);
            assertEquals(numeroOsservatori + 1, curatoreServiceImpl.getNumeroOsservatori(curatore));

            curatoreServiceImpl.aggiungiOsservatore(curatore, contributor2);
            assertEquals(numeroOsservatori + 2, curatoreServiceImpl.getNumeroOsservatori(curatore));


            curatoreServiceImpl.valuta(curatore, puntoInteresse, Stato.APPROVATO);
            assertTrue(puntoInteresse.getStato().asBoolean());
            MaterialeGenerico materialeGenerico1 = new Foto(contributor);
            assertFalse(materialeGenerico1.getStato().asBoolean());
            curatoreServiceImpl.valuta(curatore, materialeGenerico1, Stato.APPROVATO);
            poiService.aggiungiMateriale(turistaAutenticato, puntoInteresse, materialeGenerico1);
            curatoreServiceImpl.rimuoviOsservatore(curatore, contributor1);
            assertEquals(numeroOsservatori + 1, curatoreServiceImpl.getNumeroOsservatori(curatore));
        }
    }

    /*
     * Test per le varie possibilità di gestione di un itinerario
     */
    @Test
    @Order(2)
    public void testItinerario() {
        Comune comune = comuneService.creaComune("Milano");

        /*
         * Creazione di un itenerario, aggiunta di una tappa e test.
         * Aggiunti di ulteriori tappe e test
         */

        {
            int numeroItinerariIniziale = itinerarioService.findAllByComune(comune).size();
            TuristaAutenticato turistaTemp = gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR_AUTORIZZATO, "Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "5Unica@", "user5");
            if (!(turistaTemp instanceof ContributorAutorizzato contributorAutorizzato))
                throw new IllegalArgumentException("errore");

            PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse("farmacia", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), TipologiaPuntoInteresse.SALUTE_E_BENESSERE, contributorAutorizzato);
            PuntoInteresse puntoInteresse2 = poiService.creaPuntoInteresse("centro Commerciale", new Punto(comune.getPosizione().getLatitudine() - 0.02, comune.getPosizione().getLongitudine() - 0.02), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributorAutorizzato);


            Itinerario itinerario1 = contributorAutorizzatoServiceImpl.aggiungiItinerario(comune, "girodeibar", puntoInteresse, puntoInteresse2);

            assertEquals(numeroItinerariIniziale + 1, itinerarioService.findAllByComune(comune).size());
            assertEquals(2, itinerarioService.getNumeroTappe(itinerario1));

            PuntoInteresse nuovoPunto1 = poiService.creaPuntoInteresse( "universita'", new Punto(comune.getPosizione().getLatitudine() + 0.014, comune.getPosizione().getLongitudine() + 0.014), TipologiaPuntoInteresse.FORMAZIONE, contributorAutorizzato);
            PuntoInteresse nuovoPunto2 = poiService.creaPuntoInteresse( "universita2'", new Punto(comune.getPosizione().getLatitudine() + 0.014, comune.getPosizione().getLongitudine() + 0.014), TipologiaPuntoInteresse.FORMAZIONE, contributorAutorizzato);
            PuntoInteresse nuovoPunto3 = poiService.creaPuntoInteresse( "universita3'", new Punto(comune.getPosizione().getLatitudine() + 0.014, comune.getPosizione().getLongitudine() + 0.014), TipologiaPuntoInteresse.FORMAZIONE, contributorAutorizzato);


            assertTrue(contributorAutorizzatoServiceImpl.aggiungiTappaItinerario(itinerario1, nuovoPunto1));
            //itinerarioService.aggiungiTappa(itinerario1, nuovoPunto);

            assertEquals(numeroItinerariIniziale + 1, itinerarioService.findAllByComune(comune).size());
            assertEquals(3, itinerarioService.getNumeroTappe(itinerario1));

            contributorAutorizzatoServiceImpl.aggiungiTappaItinerario(itinerario1, nuovoPunto2, nuovoPunto3);
            //itinerarioService.aggiungiTappa(itinerario1, nuovoPunto1, nuovoPunto2, nuovoPunto3);
            assertEquals(numeroItinerariIniziale + 1, itinerarioService.findAllByComune(comune).size());

            assertEquals(5, itinerarioService.getNumeroTappe(itinerario1));

        }
    }

    /*
     * Test relativo alla gestione dei Contest
     */

    @Test
    @Order(3)
    public void testContest() {
        Comune comune = comuneService.creaComune("Milano");

        TuristaAutenticato turistaTemp = gestorePiattaformaService.registra(comune, Ruolo.ANIMATORE, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.APRIL, 7), "6Unica@", "user6");
        if (!(turistaTemp instanceof Animatore animatore))
            throw new IllegalArgumentException("errore");
        int numeroContestCreatiDaAnimatore = contestService.getContestByCreatore(animatore).size();
        /*
         * Creazione di un nuovo contesto libero e successivo join del turista al contest
         * aggiunta di un materiale al contest dal turistaLoggato
         */
        {

            Contest contest = animatoreServiceImpl.creaContest(animatore, "Monumento", "Foto più bella", true);
            assertEquals(numeroContestCreatiDaAnimatore + 1, contestService.getContestByCreatore(animatore).size());

            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(null, Ruolo.TURISTA, "andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "7Unica@", "user7");

            turistaAutenticatoService.partecipaAlContest(contest, turistaAutenticato);

            contestService.aggiungiMateriale(new Foto(turistaAutenticato), contest, turistaAutenticato);


            assertEquals(1, contestService.getPartecipanti(contest).size());
            assertEquals(1, contestService.getMaterialiContest(contest).size());
        }

        /*
         * Creazione di un contest privato, quindi su invito.
         * Invio dell'invito da parte dell'animatore
         * Verifica della validità dell'invito
         * Accettazione invito da parte del turista
         */
        {
            Contest contest = animatoreServiceImpl.creaContest(animatore, "Contest", "Foto più bella", false);
            assertEquals(numeroContestCreatiDaAnimatore + 2, contestService.getContestByCreatore(animatore).size());

            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(null, Ruolo.TURISTA, "andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.NOVEMBER, 5), "8Unica@", "user8");
            Invito invito = animatoreServiceImpl.invitaContest(animatore, contest, turistaAutenticato);

            assertTrue(invitoService.isValid(invito));
            turistaAutenticatoService.accettaInvitoContest(turistaAutenticato, invitoService.getInvitiRicevuti(turistaAutenticato).getLast());

            assertEquals(1, contestService.getPartecipanti(contest).size());
            // assertEquals(contestService.getContestByCreatore(animatore).getLast(),
            // contestService.getContestByPartecipante(turistaAutenticato).getLast());
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
        Comune comune = comuneService.creaComune("Milano");
        TuristaAutenticato turistaTemp = gestorePiattaformaService.registra(comune, Ruolo.ANIMATORE, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.OCTOBER, 1), "9Unica@", "user9");
        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(null, Ruolo.TURISTA, "andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.DECEMBER, 3), "10Unica@", "user10");

        if (!(turistaTemp instanceof Animatore animatore))
            throw new IllegalArgumentException("errore");


        Contest contest = animatoreServiceImpl.creaContest(animatore, "monumento", "Foto più bella", true);

        Descrizione descrizione = new Descrizione(turistaAutenticato);
        assertThrows(ContestException.class, () -> contestService.aggiungiMateriale(descrizione, contest, turistaAutenticato));

        turistaAutenticatoService.partecipaAlContest(contest, turistaAutenticato);
        MaterialeGenerico materialeGenerico = new Descrizione(turistaAutenticato);
        contestService.aggiungiMateriale(descrizione, contest, turistaAutenticato);
        assertFalse(materialeGenerico.getStato().asBoolean());
        assertEquals(1, contestService.getMaterialiContest(contest).size());
        assertTrue(animatoreServiceImpl.approvaMateriale(animatore, contest, materialeGenerico, Stato.APPROVATO));
        assertTrue(materialeGenerico.getStato().asBoolean());

    }

    /*
     * Test relativo all'eliminazione di un contenuto, che può essere un POI, un materiale, un itinerario e un contest
     */
    @Test
    @Order(5)
    public void eliminaContenuto() {

        Comune comune = comuneService.creaComune("Milano");
        int numeroPuntiInteresse = comuneService.getPuntiInteresseNelComune(comune.getNome()).size();
        TuristaAutenticato turistaTemp = gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 5), "11Unica@", "user11");
        if (!(turistaTemp instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");

        TuristaAutenticato turistaTemp2 = gestorePiattaformaService.registra(comune, Ruolo.CURATORE, "Leonardo", "rosso", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "12Unica@", "user12");
        if (!(turistaTemp2 instanceof Curatore curatore))
            throw new IllegalArgumentException("errore");

        TuristaAutenticato turistaTemp3 = gestorePiattaformaService.registra(comune, Ruolo.ANIMATORE, "Fede", "Verde", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "13Unica@", "user13");
        if (!(turistaTemp3 instanceof Animatore animatore))
            throw new IllegalArgumentException("errore");

        TuristaAutenticato turista = gestorePiattaformaService.registra(null, Ruolo.TURISTA, "aldo", "neri", new GregorianCalendar(2002, GregorianCalendar.NOVEMBER, 12), "14Unica@", "user14");

        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse( "parcheggio centrale", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), TipologiaPuntoInteresse.PARCHEGGIO, contributor);
        PuntoInteresse puntoInt2 = poiService.creaPuntoInteresse( "parcheggio centrale sotto", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), TipologiaPuntoInteresse.PARCHEGGIO, contributor);

        assertEquals(numeroPuntiInteresse + 2, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());
        curatoreServiceImpl.valuta(curatore, puntoInteresse, Stato.APPROVATO);
        curatoreServiceImpl.valuta(curatore, puntoInt2, Stato.APPROVATO);

        turistaAutenticatoService.aggiungiPreferito(turista, puntoInteresse);
        turistaAutenticatoService.aggiungiPreferito(turista, puntoInt2);

        assertEquals(2, turistaAutenticatoService.findPreferiti(turista).size());

        curatoreServiceImpl.elimina(curatore, puntoInteresse);

        assertEquals(numeroPuntiInteresse + 1, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());

        assertEquals(1, turistaAutenticatoService.findPreferiti(turista).size());

        PuntoInteresse puntoInteresse1 = poiService.creaPuntoInteresse( "parco", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), TipologiaPuntoInteresse.PARCO, contributor);

        int numeroItinerariComune = itinerarioService.findAllByComune(comune).size();
        assertThrows(IllegalArgumentException.class, () -> contributorService.aggiungiItinerario(comune, "girodeibar", puntoInteresse1));
        assertEquals(numeroItinerariComune, itinerarioService.findAllByComune(comune).size());

        curatoreServiceImpl.valuta(curatore, puntoInteresse1, Stato.toStatus(true));
        Itinerario itinerario2 = contributorService.aggiungiItinerario(comune, "giro dei bar", puntoInteresse1);
        assertEquals(numeroItinerariComune + 1, itinerarioService.findAllByComune(comune).size());
        curatoreServiceImpl.elimina(curatore, itinerario2);
        assertEquals(numeroItinerariComune, itinerarioService.findAllByComune(comune).size());


        int numeroContest = contestService.getContestByCreatore(animatore).size();
        animatoreServiceImpl.creaContest(animatore, "contest", "spiaggia", true);
        assertEquals(numeroContest + 1, contestService.getContestByCreatore(animatore).size());
        curatoreServiceImpl.elimina(curatore, contestService.getContestByCreatore(animatore).getLast());
        assertEquals(numeroContest, contestService.getContestByCreatore(animatore).size());

        Itinerario itinerario3 = contributorService.aggiungiItinerario(comune, "girodeibar2", puntoInteresse1);
        assertEquals(1, itinerarioService.getNumeroTappe(itinerario3));
        assertTrue(contributorService.aggiungiTappaItinerario(itinerario3, puntoInt2));
        assertEquals(2, itinerarioService.getNumeroTappe(itinerario3));
        curatoreServiceImpl.rimuoviTappa(curatore, itinerario3, puntoInteresse1);
        assertEquals(1, itinerarioService.getNumeroTappe(itinerario3));


        PuntoInteresse puntoInteresse2 = poiService.creaPuntoInteresse( "Castello", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), TipologiaPuntoInteresse.MONUMENTO, contributor);
        curatoreServiceImpl.valuta(curatore, puntoInteresse2, Stato.APPROVATO);
        MaterialeGenerico foto = new Foto(turista);
        curatoreServiceImpl.valuta(curatore, foto, Stato.APPROVATO);

        poiService.aggiungiMateriale(turista, puntoInteresse2, foto);
        assertEquals(1, poiService.getMaterialiPoi(puntoInteresse2).size());
        curatoreServiceImpl.elimina(curatore, foto);
        assertEquals(0, poiService.getMaterialiPoi(puntoInteresse2).size());

    }


    /*
     * Test per modificare la scadenza di un contentuto
     */

    @Test
    @Order(6)
    public void modificaScadenzaContenuto() {
        Comune comune = comuneService.creaComune("Milano");
        TuristaAutenticato turistaTemp = gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "15Unica@", "user15");
        if (!(turistaTemp instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");

        int numPoi = poiService.findActive().size();
        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse( "Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributor);
        assertEquals(numPoi + 1, poiService.findActive().size());
        assertEquals(LocalDate.MAX, puntoInteresse.getExpireDate());


        contributorService.modificaScadenza(puntoInteresse, LocalDate.of(2024, 2, 1));
        assertEquals(LocalDate.of(2024, 2, 1), puntoInteresse.getExpireDate());
        assertEquals(numPoi, poiService.findActive().size());
    }
}