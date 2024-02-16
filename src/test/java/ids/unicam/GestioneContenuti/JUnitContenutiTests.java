package ids.unicam.GestioneContenuti;

import ids.unicam.Comune;
import ids.unicam.DataBase.GestoreDatabase;
import ids.unicam.exception.ContestException;
import ids.unicam.models.Orario;
import ids.unicam.models.Ruolo;
import ids.unicam.models.Service.*;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.*;
import ids.unicam.utilites.DayOfWeek;
import ids.unicam.utilites.Punto;
import ids.unicam.utilites.Stato;
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
    private final ComuneService comuneService;
    private final ContributorService contributorService;
    private final ContributorAutorizzatoService contributorAutorizzatoService;
    private final CuratoreService curatoreService;
    private final AnimatoreService animatoreService;
    private final TuristaAutenticatoService turistaAutenticatoService;
    private final PoiService poiService;
    private final ItinerarioService itinerarioService;
    private final MaterialeService materialeService;
    private final ContestService contestService;
    private final GestorePiattaformaService gestorePiattaformaService;
    private final InvitoService invitoService;
    private final GestoreDatabase gestoreDatabase;

    @Autowired
    public JUnitContenutiTests(ComuneService comuneService, ContributorService contributorService, ContributorAutorizzatoService contributorAutorizzatoService, CuratoreService curatoreService, AnimatoreService animatoreService, TuristaAutenticatoService turistaAutenticatoService, PoiService poiService, ItinerarioService itinerarioService, MaterialeService materialeService, ContestService contestService, GestorePiattaformaService gestorePiattaformaService, InvitoService invitoService, GestoreDatabase gestoreDatabase) {
        this.comuneService = comuneService;
        this.contributorService = contributorService;
        this.contributorAutorizzatoService = contributorAutorizzatoService;
        this.curatoreService = curatoreService;
        this.animatoreService = animatoreService;
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.poiService = poiService;
        this.itinerarioService = itinerarioService;
        this.materialeService = materialeService;
        this.contestService = contestService;
        this.gestorePiattaformaService = gestorePiattaformaService;
        this.invitoService = invitoService;
        this.gestoreDatabase = gestoreDatabase;
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
        TuristaAutenticato turistaTemp = gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR,  "Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "1Unica@", "user1");
        if(!(turistaTemp instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");
        /*
         * Creazione di un Punto di Interesse da parte di un contributor, che di base non è approvato
         */
        {
            int puntiInteresseComuneIniziali = comuneService.getPuntiInteresseNelComune(comune.getNome()).size();
            Orario orario = new Orario();
            orario.setOrarioApertura(DayOfWeek.MONDAY, LocalTime.of(8, 30), LocalTime.of(18, 0));
            PuntoInteresse punto1 = new PuntoInteresse(comune, "bar", new Punto(comune.getPosizione().getLatitudine() + 0.01, comune.getPosizione().getLongitudine() + 0.01), orario, TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE);
            contributorService.aggiungiPuntoInteresse(contributor, punto1);

            assertEquals(puntiInteresseComuneIniziali + 1, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());
            assertFalse(punto1.getStato().asBoolean());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted, che di base è approvato
         */
        {
            gestorePiattaformaService.promuovi(contributor, Ruolo.CONTRIBUTOR_AUTORIZZATO);

            ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiByComune(comune.getNome()).getLast();
            int puntiInteresseComuneIniziali = comuneService.getPuntiInteresseNelComune(comune.getNome()).size();

            PuntoInteresse punto2 = (new PuntoInteresse(comune, "bar2", new Punto(comune.getPosizione().getLatitudine() + 0.02, comune.getPosizione().getLongitudine() + 0.02), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE));

            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato, punto2);
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

            ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiByComune(comune.getNome()).getLast();
            assertThrows(IllegalArgumentException.class, () -> new PuntoInteresse(comune, "chiesa", new Punto(comune.getPosizione().getLatitudine() + 2, comune.getPosizione().getLongitudine() + 2), TipologiaPuntoInteresse.LUOGO_DI_CULTO));

        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted
         * aggiunta di un materiale al punto di interesse
         * verifica finale
         */
        {
            ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiByComune(comune.getNome()).getFirst();
            PuntoInteresse puntoInteresse = new PuntoInteresse(comune, "Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE);
            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato, puntoInteresse);

            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(null, Ruolo.TURISTA, "andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.FEBRUARY, 3), "2Unica@", "user2");
            MaterialeGenerico materialeGenerico = new Foto(turistaAutenticato);
            poiService.creaMateriale(contributorAutorizzato, puntoInteresse, materialeGenerico);
            assertEquals(1, materialeService.findByWhere(puntoInteresse).size());
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
            TuristaAutenticato turistaTemp1 = gestorePiattaformaService.registra(comune, Ruolo.CURATORE,  "Peppe", "Peppe", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "4Unica@", "user4");
            if(!(turistaTemp1 instanceof Curatore curatore))
                throw new IllegalArgumentException("errore");

            Orario orarioAccademia = new Orario();
            orarioAccademia.setOrarioApertura(DayOfWeek.MONDAY, LocalTime.of(9, 0), LocalTime.of(18, 0));

            PuntoInteresse puntoInteresse = contributorService.aggiungiPuntoInteresse(contributor, new PuntoInteresse(comune, "Accademia", new Punto(comune.getPosizione().getLatitudine() + 0.01, comune.getPosizione().getLongitudine() + 0.01), orarioAccademia, TipologiaPuntoInteresse.CENTRO_SPORTIVO));

            assertFalse(puntoInteresse.getStato().asBoolean());
            TuristaAutenticato turistaTemp2 = gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR,  "Peppe", "Paol", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "4Unica@", "user44");
            if(!(turistaTemp2 instanceof Contributor contributor1))
                throw new IllegalArgumentException("errore");
            TuristaAutenticato turistaTemp3 = gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR,  "Pietro", "Pier", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "4Unica@", "user45");
            if(!(turistaTemp3 instanceof Contributor contributor2))
                throw new IllegalArgumentException("errore");

            int numeroOsservatori=curatoreService.getOsservatori(curatore).size();
            curatoreService.aggiungiOsservatore(curatore,contributor1);
            assertEquals(numeroOsservatori+1, curatoreService.getNumeroOsservatori(curatore));

            curatoreService.aggiungiOsservatore(curatore,contributor2);
            assertEquals(numeroOsservatori+2, curatoreService.getNumeroOsservatori(curatore));



            curatoreService.valuta(curatore,puntoInteresse, Stato.APPROVED);
            assertTrue(puntoInteresse.getStato().asBoolean());
            MaterialeGenerico materialeGenerico1 = new Foto(contributor);
            assertFalse(materialeGenerico1.getStato().asBoolean());
            curatoreService.valuta(curatore,materialeGenerico1, Stato.APPROVED);
            poiService.creaMateriale(turistaAutenticato, puntoInteresse, materialeGenerico1);
            curatoreService.rimuoviOsservatore(curatore,contributor1);
            assertEquals(numeroOsservatori+1, curatoreService.getNumeroOsservatori(curatore));
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
            TuristaAutenticato turistaTemp =gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR_AUTORIZZATO,  "Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "5Unica@", "user5");
            if(!(turistaTemp instanceof ContributorAutorizzato contributorAutorizzato))
                throw new IllegalArgumentException("errore");

            PuntoInteresse puntoInteresse = new PuntoInteresse(comune, "farmacia", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), TipologiaPuntoInteresse.SALUTE_E_BENESSERE);
            PuntoInteresse puntoInteresse2 = new PuntoInteresse(comune, "centro Commerciale", new Punto(comune.getPosizione().getLatitudine() - 0.02, comune.getPosizione().getLongitudine() - 0.02), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE);


            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato, puntoInteresse);
            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato, puntoInteresse2);

            Itinerario itinerario1 = contributorAutorizzatoService.aggiungiItinerario(comune, "girodeibar", puntoInteresse, puntoInteresse2);

            assertEquals(numeroItinerariIniziale + 1, itinerarioService.findAllByComune(comune).size());
            assertEquals(2, itinerarioService.getNumeroTappe(itinerario1));

            PuntoInteresse nuovoPunto = new PuntoInteresse(comune, "universita'", new Punto(comune.getPosizione().getLatitudine() + 0.014, comune.getPosizione().getLongitudine() + 0.014), TipologiaPuntoInteresse.FORMAZIONE);
            PuntoInteresse nuovoPunto1 = new PuntoInteresse(comune, "universita1'", new Punto(comune.getPosizione().getLatitudine() + 0.014, comune.getPosizione().getLongitudine() + 0.014), TipologiaPuntoInteresse.FORMAZIONE);
            PuntoInteresse nuovoPunto2 = new PuntoInteresse(comune, "universita2'", new Punto(comune.getPosizione().getLatitudine() + 0.014, comune.getPosizione().getLongitudine() + 0.014), TipologiaPuntoInteresse.FORMAZIONE);
            PuntoInteresse nuovoPunto3 = new PuntoInteresse(comune, "universita3'", new Punto(comune.getPosizione().getLatitudine() + 0.014, comune.getPosizione().getLongitudine() + 0.014), TipologiaPuntoInteresse.FORMAZIONE);

            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato, nuovoPunto);

            contributorAutorizzatoService.aggiungiTappaItinerario(itinerario1, nuovoPunto1);
            //itinerarioService.aggiungiTappa(itinerario1, nuovoPunto);

            assertEquals(numeroItinerariIniziale + 1, itinerarioService.findAllByComune(comune).size());
            assertEquals(3, itinerarioService.getNumeroTappe(itinerario1));

            contributorAutorizzatoService.aggiungiTappaItinerario(itinerario1,  nuovoPunto2, nuovoPunto3);
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

        TuristaAutenticato turistaTemp =gestorePiattaformaService.registra(comune, Ruolo.ANIMATORE,  "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.APRIL, 7), "6Unica@", "user6");
        if(!(turistaTemp instanceof Animatore animatore))
            throw new IllegalArgumentException("errore");
        int numeroContestCreatiDaAnimatore = contestService.getContestByCreatore(animatore).size();
        /*
         * Creazione di un nuovo contesto libero e successivo join del turista al contest
         * aggiunta di un materiale al contest dal turistaLoggato
         */
        {

            Contest contest = animatoreService.creaContest(animatore, "Monumento", "Foto più bella", true);
            assertEquals(numeroContestCreatiDaAnimatore + 1, contestService.getContestByCreatore(animatore).size());

            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(null, Ruolo.TURISTA, "andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "7Unica@", "user7");

            turistaAutenticatoService.partecipaAlContest(contest, turistaAutenticato);

            MaterialeGenerico materialeGenerico = contestService.aggiungiMateriale(new Foto(turistaAutenticato), contest, turistaAutenticato);


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
            Contest contest = animatoreService.creaContest(animatore, "Contest", "Foto più bella", false);
            assertEquals(numeroContestCreatiDaAnimatore + 2, contestService.getContestByCreatore(animatore).size());

            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(null, Ruolo.TURISTA, "andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.NOVEMBER, 5), "8Unica@", "user8");
            animatoreService.invitaContest(animatore, contest, turistaAutenticato);

            assertTrue(invitoService.isValid(invitoService.getInvitiRicevuti(turistaAutenticato).getLast()));
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
        TuristaAutenticato turistaTemp =gestorePiattaformaService.registra(comune, Ruolo.ANIMATORE,  "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.OCTOBER, 1), "9Unica@", "user9");
        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(null, Ruolo.TURISTA,"andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.DECEMBER, 3), "10Unica@", "user10");

        if(!(turistaTemp instanceof Animatore animatore))
            throw new IllegalArgumentException("errore");



        Contest contest = animatoreService.creaContest(animatore, "monumento", "Foto più bella", true);

        Descrizione descrizione = new Descrizione(turistaAutenticato);
        assertThrows(ContestException.class, () -> contestService.aggiungiMateriale(descrizione, contest, turistaAutenticato));

        turistaAutenticatoService.partecipaAlContest(contest, turistaAutenticato);
        MaterialeGenerico materialeGenerico = contestService.aggiungiMateriale(descrizione, contest, turistaAutenticato);
        assertFalse(materialeGenerico.getStato().asBoolean());
        assertEquals(1, contestService.getMaterialiContest(contest).size());
        assertTrue(animatoreService.approvaMateriale(animatore, contest, materialeGenerico, Stato.APPROVED));
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
        TuristaAutenticato turistaTemp =gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR,  "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 5), "11Unica@", "user11");
        if(!(turistaTemp instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");

        TuristaAutenticato turistaTemp2 = gestorePiattaformaService.registra(comune, Ruolo.CURATORE,  "Leonardo", "rosso", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "12Unica@", "user12");
        if(!(turistaTemp2 instanceof Curatore curatore))
            throw new IllegalArgumentException("errore");

        TuristaAutenticato turistaTemp3 = gestorePiattaformaService.registra(comune, Ruolo.ANIMATORE,  "Fede", "Verde", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "13Unica@", "user13");
        if(!(turistaTemp3 instanceof Animatore animatore))
            throw new IllegalArgumentException("errore");

        TuristaAutenticato turista = gestorePiattaformaService.registra(null, Ruolo.TURISTA, "aldo", "neri", new GregorianCalendar(2002, GregorianCalendar.NOVEMBER, 12), "14Unica@", "user14");

        PuntoInteresse puntoInteresse = contributorService.aggiungiPuntoInteresse(contributor, new PuntoInteresse(comune, "parcheggio centrale", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), TipologiaPuntoInteresse.PARCHEGGIO));
        PuntoInteresse puntoInt2 = contributorService.aggiungiPuntoInteresse(contributor, new PuntoInteresse(comune, "parcheggio centrale sotto", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), TipologiaPuntoInteresse.PARCHEGGIO));

        assertEquals(numeroPuntiInteresse + 2, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());
        curatoreService.valuta(curatore,puntoInteresse, Stato.APPROVED);
        curatoreService.valuta(curatore,puntoInt2, Stato.APPROVED);

        turistaAutenticatoService.aggiungiPreferito(turista, puntoInteresse);
        turistaAutenticatoService.aggiungiPreferito(turista, puntoInt2);

        assertEquals(2, turistaAutenticatoService.findPreferiti(turista).size());

        curatoreService.elimina(puntoInteresse);

        assertEquals(numeroPuntiInteresse + 1, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());

        assertEquals(1, turistaAutenticatoService.findPreferiti(turista).size());

        PuntoInteresse puntoInteresse1 = contributorService.aggiungiPuntoInteresse(contributor, new PuntoInteresse(comune, "parco", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), TipologiaPuntoInteresse.PARCO));

        int numeroItinerariComune = itinerarioService.findAllByComune(comune).size();
        assertThrows(IllegalArgumentException.class, () -> contributorService.aggiungiItinerario(comune, "girodeibar", puntoInteresse1));
        assertEquals(numeroItinerariComune, itinerarioService.findAllByComune(comune).size());

        curatoreService.valuta(curatore,puntoInteresse1, Stato.toStatus(true));
        Itinerario itinerario2 = contributorService.aggiungiItinerario(comune, "giro dei bar", puntoInteresse1);
        assertEquals(numeroItinerariComune + 1, itinerarioService.findAllByComune(comune).size());
        curatoreService.elimina(itinerario2);
        assertEquals(numeroItinerariComune, itinerarioService.findAllByComune(comune).size());


        int numeroContest = contestService.getContestByCreatore(animatore).size();
        animatoreService.creaContest(animatore, "contest", "spiaggia", true);
        assertEquals(numeroContest + 1, contestService.getContestByCreatore(animatore).size());
        curatoreService.elimina(contestService.getContestByCreatore(animatore).getLast());
        assertEquals(numeroContest, contestService.getContestByCreatore(animatore).size());

        Itinerario itinerario3 = contributorService.aggiungiItinerario(comune, "girodeibar2", puntoInteresse1);
        assertEquals(1, itinerarioService.getNumeroTappe(itinerario3));
        assertTrue(contributorService.aggiungiTappaItinerario(itinerario3, puntoInt2));
        assertEquals(2, itinerarioService.getNumeroTappe(itinerario3));
        curatoreService.rimuoviTappa(curatore, itinerario3, puntoInteresse1);
        assertEquals(1, itinerarioService.getNumeroTappe(itinerario3));


        PuntoInteresse puntoInteresse2 = contributorService.aggiungiPuntoInteresse(contributor, new PuntoInteresse(comune, "Castello", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), TipologiaPuntoInteresse.MONUMENTO));
        curatoreService.valuta(curatore,puntoInteresse2, Stato.APPROVED);
        MaterialeGenerico foto = new Foto(turista);
        curatoreService.valuta(curatore,foto, Stato.APPROVED);

        poiService.creaMateriale(turista, puntoInteresse2, foto);
        assertEquals(1, materialeService.findByWhere(puntoInteresse2).size());
        curatoreService.elimina(curatore, foto);
        assertEquals(0, materialeService.findByWhere(puntoInteresse2).size());

    }


    /*
     * Test per modificare la scadenza di un contentuto
     */

    @Test
    @Order(6)
    public void modificaScadenzaContenuto() {
        Comune comune = comuneService.creaComune("Milano");
        TuristaAutenticato turistaTemp =gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR,  "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "15Unica@", "user15");
        if(!(turistaTemp instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");

        int numPoi=poiService.findAll().size();
        PuntoInteresse puntoInteresse = new PuntoInteresse(comune, "Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE);
        contributorService.aggiungiPuntoInteresse(contributor, puntoInteresse);
        assertEquals(numPoi+1,poiService.findAll().size());
        assertEquals(LocalDate.MAX, puntoInteresse.getExpireDate());


        contributorService.modificaScandenza(puntoInteresse, LocalDate.of(2024, 2, 1));
        assertEquals(LocalDate.of(2024, 2, 1),puntoInteresse.getExpireDate());
        assertEquals(numPoi,poiService.findAll().size());
    }
}