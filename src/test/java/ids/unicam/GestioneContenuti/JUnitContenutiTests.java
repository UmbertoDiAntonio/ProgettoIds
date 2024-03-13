package ids.unicam.GestioneContenuti;

import ids.unicam.DataBase.GestoreDatabase;
import ids.unicam.Service.impl.*;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.exception.ContestException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Comune;
import ids.unicam.models.DTO.ContributorDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.Invito;
import ids.unicam.models.Punto;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.RuoloRegistrazione;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.materiali.TipologiaMateriale;
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
import java.util.Calendar;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
public class JUnitContenutiTests {
    private final ComuneServiceImpl comuneService;
    private final CuratoreServiceImpl curatoreServiceImpl;
    private final AnimatoreServiceImpl animatoreServiceImpl;
    private final TuristaAutenticatoServiceImpl turistaAutenticatoService;
    private final PoiServiceImpl poiService;
    private final ItinerarioServiceImpl itinerarioService;
    private final ContestServiceImpl contestService;
    private final GestorePiattaformaServiceImpl gestorePiattaformaService;
    private final InvitoServiceImpl invitoService;
    private final MaterialeServiceImpl materialeService;

    @Autowired
    public JUnitContenutiTests(ComuneServiceImpl comuneService, CuratoreServiceImpl curatoreServiceImpl, AnimatoreServiceImpl animatoreServiceImpl, TuristaAutenticatoServiceImpl turistaAutenticatoService, PoiServiceImpl poiService, ItinerarioServiceImpl itinerarioService, ContestServiceImpl contestService, GestorePiattaformaServiceImpl gestorePiattaformaService, InvitoServiceImpl invitoService, GestoreDatabase gestoreDatabase, MaterialeServiceImpl materialeService) {
        this.comuneService = comuneService;
        this.curatoreServiceImpl = curatoreServiceImpl;
        this.animatoreServiceImpl = animatoreServiceImpl;
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.poiService = poiService;
        this.itinerarioService = itinerarioService;
        this.contestService = contestService;
        this.gestorePiattaformaService = gestorePiattaformaService;
        this.invitoService = invitoService;
        this.materialeService = materialeService;
        gestoreDatabase.eliminaTabelleDB();
        gestoreDatabase.inizializzaDatabase();
    }


    /*
     * Test relativo al corretto passaggio delle coordinate
     */
    @Test
    @Order(0)
    public void testCoordinate() throws ConnessioneFallitaException {
        {
            Punto first = new Punto(0, 0);
            Punto second = new Punto(10, 0);
            assertEquals(10, first.getDistanza(second), 0);
            assertEquals(100, first.getDistanzaAlQuadrato(second), 0);
            Comune milano = comuneService.creaComune("Milano", "admin");
            Comune roma = comuneService.creaComune("Roma", "admin");
            Comune napoli = comuneService.creaComune("Napoli", "admin");

            assertEquals(2, comuneService.find(comune -> comune.getPosizione().getDistanza(milano.getPosizione()) < 5).size());
            assertEquals(3, comuneService.find(comune -> comune.getPosizione().getDistanza(roma.getPosizione()) < 5).size());
            assertEquals(2, comuneService.find(comune -> comune.getPosizione().getDistanza(napoli.getPosizione()) < 5).size());
        }
    }

    /*
     * Test per creare un punto di interesse
     */
    @Test
    @Order(1)
    public void testPoi() throws ConnessioneFallitaException, FuoriComuneException, ContestException {
        Comune comune = comuneService.creaComune("Milano", "admin");

        TuristaAutenticato turistaTemp = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("Mario", "Rossi", LocalDate.of(2000, 3, 17), "1Unica@", "user1")), RuoloRegistrazione.CONTRIBUTOR);

        if (!(turistaTemp instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");
        /*
         * Creazione di un Punto di Interesse da parte di un contributor, che di base non è approvato
         */
        {
            int puntiInteresseComuneIniziali = poiService.find(puntoInteresse -> puntoInteresse.getComune().equals(comune)).size();
            Orario orario = new Orario();
            orario.setOrarioApertura(DayOfWeek.MONDAY, LocalTime.of(8, 30), LocalTime.of(18, 0));
            PuntoInteresse punto1 = poiService.creaPuntoInteresse("bar", new Punto(comune.getPosizione().getLatitudine() + 0.01, comune.getPosizione().getLongitudine() + 0.01), orario, TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributor.getUsername());

            assertEquals(puntiInteresseComuneIniziali, poiService.find(puntoInteresse -> puntoInteresse.getComune().equals(comune)).size());
            assertNull(punto1.getStato().asBoolean());

        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted, che di base è approvato
         */
        {

            gestorePiattaformaService.cambiaRuolo("admin", contributor.getUsername(), Ruolo.CONTRIBUTOR_AUTORIZZATO);
            ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiDelComune(comune.getNome(), "admin").getLast();
            int puntiInteresseComuneIniziali = poiService.find(puntoInteresse -> puntoInteresse.getComune().equals(comune)).size();

            PuntoInteresse punto2 = poiService.creaPuntoInteresse("bar2", new Punto(comune.getPosizione().getLatitudine() + 0.02, comune.getPosizione().getLongitudine() + 0.02), new Orario(), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributorAutorizzato.getUsername());

            assertEquals(puntiInteresseComuneIniziali + 1, poiService.find(puntoInteresse -> puntoInteresse.getComune().equals(comune)).size());
            assertEquals(Boolean.TRUE, punto2.getStato().asBoolean());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted, che di base è approvato, ma fuori dall'area del comune
         * quindi non può essere creato
         */
        {
            int puntiInteresseComuneIniziali = poiService.find(puntoInteresse -> puntoInteresse.getComune().equals(comune)).size();
            Comune comune2 = comuneService.creaComune("Roma", "admin");

            TuristaAutenticato turistaTemp2 = gestorePiattaformaService.registra(new ContributorDTO(comune2, new TuristaAutenticatoDTO("Mario", "Rossi", LocalDate.of(2000, Calendar.MARCH, 17), "1Unica@", "user19")), RuoloRegistrazione.CONTRIBUTOR);
            turistaTemp2 = gestorePiattaformaService.cambiaRuolo("admin", turistaTemp2.getUsername(), Ruolo.CONTRIBUTOR_AUTORIZZATO);

            if (!(turistaTemp2 instanceof ContributorAutorizzato contributorAutorizzato))
                throw new IllegalArgumentException("errore");

            assertThrows(FuoriComuneException.class, () -> poiService.creaPuntoInteresse("chiesa", new Punto(comune.getPosizione().getLatitudine() + 2, comune.getPosizione().getLongitudine() + 2), new Orario(), TipologiaPuntoInteresse.LUOGO_DI_CULTO, contributorAutorizzato.getUsername()));
            assertEquals(puntiInteresseComuneIniziali, poiService.find(puntoInteresse -> puntoInteresse.getComune().equals(comune)).size());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted
         * aggiunta di un materiale al punto di interesse
         * verifica finale
         */
        {
            ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiDelComune(comune.getNome(), "admin").getFirst();
            PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributorAutorizzato.getUsername());

            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(new ContributorDTO(null, new TuristaAutenticatoDTO("andrea", "neri", LocalDate.of(2000, Calendar.FEBRUARY, 3), "2Unica@", "user2")), RuoloRegistrazione.TURISTA);

            MaterialeGenerico materialeGenerico = materialeService.crea("/fotoTest", TipologiaMateriale.FOTO, turistaAutenticato);
            turistaAutenticatoService.aggiungiMateriale(contributorAutorizzato.getUsername(), puntoInteresse.getId(), materialeGenerico);

            assertEquals(1, poiService.getMaterialiPoi(puntoInteresse.getId()).size());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted
         * aggiunta di un materiale al punto di interesse da parte di un contributor
         * approvazione da parte del curatore
         * sottoscrizione observer per la notifica
         * verifica finale
         */
        {
            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(new ContributorDTO(null, new TuristaAutenticatoDTO("andrea", "neri", LocalDate.of(2000, Calendar.FEBRUARY, 3), "3Unica@", "user3")), RuoloRegistrazione.TURISTA);
            TuristaAutenticato turistaTemp1 = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("Peppe", "Peppe", LocalDate.of(2000, Calendar.MARCH, 11), "4Unica@", "user4")), RuoloRegistrazione.CONTRIBUTOR);
            turistaTemp1 = gestorePiattaformaService.cambiaRuolo("admin", turistaTemp1.getUsername(), Ruolo.CURATORE);
            if (!(turistaTemp1 instanceof Curatore curatore))
                throw new IllegalArgumentException("errore");

            turistaTemp = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("Mario", "Rossi", LocalDate.of(2000, 3, 17), "1Unica@", "user100")), RuoloRegistrazione.CONTRIBUTOR);

            if (!(turistaTemp instanceof Contributor contributor2))
                throw new IllegalArgumentException("errore");
            Orario orarioAccademia = new Orario();
            orarioAccademia.setOrarioApertura(DayOfWeek.asDayOfWeek(1), LocalTime.of(9, 0), LocalTime.of(18, 0));
            PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse("Accademia", new Punto(comune.getPosizione().getLatitudine() + 0.0, comune.getPosizione().getLongitudine() + 0.0), orarioAccademia, TipologiaPuntoInteresse.CENTRO_SPORTIVO, contributor2.getUsername());
            assertNull(poiService.getStato(puntoInteresse.getId()).asBoolean());

            curatoreServiceImpl.valutaPuntoInteresse(curatore.getUsername(), puntoInteresse.getId(), true);

            assertThrows(UnsupportedOperationException.class, () -> curatoreServiceImpl.valutaPuntoInteresse(curatore.getUsername(), puntoInteresse.getId(), Stato.NON_APPROVATO.asBoolean()));


            assertEquals(Boolean.TRUE, poiService.getStato(puntoInteresse.getId()).asBoolean());
            MaterialeGenerico materialeGenerico1 = materialeService.crea("/testFoto", TipologiaMateriale.FOTO, contributor);
            assertNull(materialeGenerico1.getStato().asBoolean());
            turistaAutenticatoService.aggiungiMateriale(turistaAutenticato.getUsername(), puntoInteresse.getId(), materialeGenerico1);
            curatoreServiceImpl.valutaMateriale(curatore.getUsername(), materialeGenerico1.getId(), Stato.APPROVATO.asBoolean());

        }
    }

    /*
     * Test per le varie possibilità di gestione di un itinerario
     */
    @Test
    @Order(2)
    public void testItinerario() throws ConnessioneFallitaException, FuoriComuneException {
        Comune comune = comuneService.creaComune("Milano", "admin");


        /*
         * Creazione di un itenerario, aggiunta di una tappa e test.
         * Aggiunti di ulteriori tappe e test
         */

        {
            int numeroItinerariIniziale = itinerarioService.find(itinerario -> itinerario.getComune().equals(comune)).size();
            TuristaAutenticato turistaTemp = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("Mario", "Rossi", LocalDate.of(2000, Calendar.MARCH, 11), "5Unica@", "user5")), RuoloRegistrazione.CONTRIBUTOR);
            turistaTemp = gestorePiattaformaService.cambiaRuolo("admin", turistaTemp.getUsername(), Ruolo.CONTRIBUTOR_AUTORIZZATO);
            if (!(turistaTemp instanceof ContributorAutorizzato contributorAutorizzato))
                throw new IllegalArgumentException("errore");


            Itinerario itinerario1 = itinerarioService.creaItinerario(contributorAutorizzato.getUsername(), "girodeibar");

            assertEquals(numeroItinerariIniziale + 1, itinerarioService.find(itinerario -> itinerario.getComune().equals(comune)).size());
            assertEquals(0, itinerarioService.getTappe(itinerario1.getId()).size());

            PuntoInteresse nuovoPunto1 = poiService.creaPuntoInteresse("universita'", new Punto(comune.getPosizione().getLatitudine() + 0.014, comune.getPosizione().getLongitudine() + 0.014), new Orario(), TipologiaPuntoInteresse.FORMAZIONE, contributorAutorizzato.getUsername());
            PuntoInteresse nuovoPunto2 = poiService.creaPuntoInteresse("universita2'", new Punto(comune.getPosizione().getLatitudine() + 0.014, comune.getPosizione().getLongitudine() + 0.014), new Orario(), TipologiaPuntoInteresse.FORMAZIONE, contributorAutorizzato.getUsername());
            PuntoInteresse nuovoPunto3 = poiService.creaPuntoInteresse("universita3'", new Punto(comune.getPosizione().getLatitudine() + 0.014, comune.getPosizione().getLongitudine() + 0.014), new Orario(), TipologiaPuntoInteresse.FORMAZIONE, contributorAutorizzato.getUsername());


           itinerarioService.aggiungiTappa(contributorAutorizzato.getUsername(), itinerario1.getId(), nuovoPunto1.getId());

            assertEquals(numeroItinerariIniziale + 1, itinerarioService.find(itinerario -> itinerario.getComune().equals(comune)).size());
            assertEquals(1, itinerarioService.getTappe(itinerario1.getId()).size());

            itinerarioService.aggiungiTappa(contributorAutorizzato.getUsername(), itinerario1.getId(), nuovoPunto2.getId(), nuovoPunto3.getId());
            assertEquals(numeroItinerariIniziale + 1, itinerarioService.find(itinerario -> itinerario.getComune().equals(comune)).size());

            assertEquals(3, itinerarioService.getTappe(itinerario1.getId()).size());

        }
    }

    /*
     * Test relativo alla gestione dei Contest
     */

    @Test
    @Order(3)
    public void testContest() throws ConnessioneFallitaException, ContestException {
        Comune comune = comuneService.creaComune("Milano", "admin");


        TuristaAutenticato turistaTemp = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("mario", "rossi", LocalDate.of(2000, Calendar.APRIL, 7), "6Unica@", "user6")), RuoloRegistrazione.CONTRIBUTOR);
        turistaTemp = gestorePiattaformaService.cambiaRuolo("admin", turistaTemp.getUsername(), Ruolo.ANIMATORE);
        if (!(turistaTemp instanceof Animatore animatore))
            throw new IllegalArgumentException("errore");
        int numeroContestCreatiDaAnimatore = contestService.find(contest -> contest.getCreatore().equals(animatore)).size();
        /*
         * Creazione di un nuovo contesto libero e successivo join del turista al contest
         * aggiunta di un materiale al contest dal turistaLoggato
         */
        {

            Contest contest = contestService.creaContest("Monumento", "Foto più bella", animatore, true);

            assertEquals(numeroContestCreatiDaAnimatore + 1, contestService.find(contest2 -> contest2.getCreatore().equals(animatore)).size());

            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(new ContributorDTO(null, new TuristaAutenticatoDTO("andrea", "neri", LocalDate.of(2000, Calendar.MARCH, 11), "7Unica@", "user7")), RuoloRegistrazione.TURISTA);

            turistaAutenticatoService.partecipaAlContest(contest.getId(), turistaAutenticato.getUsername());

            contestService.aggiungiMateriale(turistaAutenticato.getUsername(), contest.getId(), materialeService.crea("/testFoto", TipologiaMateriale.FOTO, turistaAutenticato));


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
            Contest contest = contestService.creaContest("Monumento", "Foto più bella", animatore, false);
            assertEquals(numeroContestCreatiDaAnimatore + 2, contestService.find(contest2 -> contest2.getCreatore().equals(animatore)).size());

            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(new ContributorDTO(null, new TuristaAutenticatoDTO("andrea", "neri", LocalDate.of(2000, Calendar.NOVEMBER, 5), "8Unica@", "user8")), RuoloRegistrazione.TURISTA);
            Invito invito = animatoreServiceImpl.invitaContest(animatore.getUsername(), contest.getId(), turistaAutenticato.getUsername());


            assertTrue(invitoService.isValid(invito));

            turistaAutenticatoService.accettaInvitoContest(turistaAutenticato.getUsername(), invito.getId());

            assertEquals(1, contestService.getPartecipanti(contest).size());
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
    public void approvaMaterialeByAnimatore() throws ConnessioneFallitaException, ContestException {
        Comune comune = comuneService.creaComune("Milano", "admin");

        TuristaAutenticato turistaTemp = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("mario", "rossi", LocalDate.of(2000, Calendar.OCTOBER, 1), "9Unica@", "user9")), RuoloRegistrazione.CONTRIBUTOR);
        turistaTemp = gestorePiattaformaService.cambiaRuolo("admin", turistaTemp.getUsername(), Ruolo.ANIMATORE);

        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(new ContributorDTO(null, new TuristaAutenticatoDTO("andrea", "neri", LocalDate.of(2000, Calendar.DECEMBER, 3), "10Unica@", "user10")), RuoloRegistrazione.TURISTA);

        if (!(turistaTemp instanceof Animatore animatore))
            throw new IllegalArgumentException("errore");


        Contest contest = contestService.creaContest("Monumento", "Foto più bella", animatore, true);

        MaterialeGenerico descrizione = materialeService.crea("/testFoto", TipologiaMateriale.FOTO, turistaAutenticato);

        contestService.aggiungiMateriale(turistaAutenticato.getUsername(), contest.getId(), descrizione);


        turistaAutenticatoService.partecipaAlContest(contest.getId(), turistaAutenticato.getUsername());
        MaterialeGenerico materialeGenerico = materialeService.crea("/testFoto", TipologiaMateriale.FOTO, turistaAutenticato);

        contestService.aggiungiMateriale(turistaAutenticato.getUsername(), contest.getId(), materialeGenerico);

        assertNull(materialeGenerico.getStato().asBoolean());
        assertEquals(2, contestService.getMaterialiContest(contest).size());

        animatoreServiceImpl.approvaMateriale(animatore.getUsername(), contest.getId(), materialeGenerico.getId(), true);


        assertEquals(Boolean.TRUE, materialeService.getStato(materialeGenerico.getId()).get().asBoolean());

    }

    /*
     * Test relativo all'eliminazione di un contenuto, che può essere un POI, un materiale, un itinerario e un contest
     */
    @Test
    @Order(5)
    public void eliminaContenuto() throws ConnessioneFallitaException, FuoriComuneException, ContestException {

        Comune comune = comuneService.creaComune("Milano", "admin");

        int numeroPuntiInteresse = poiService.find(puntoInteresse -> puntoInteresse.getComune().equals(comune)).size();
        TuristaAutenticato turistaTemp = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 5), "11Unica@", "user11")), RuoloRegistrazione.CONTRIBUTOR);
        gestorePiattaformaService.cambiaRuolo("admin", turistaTemp.getUsername(), Ruolo.ANIMATORE);
        if (!(turistaTemp instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");

        TuristaAutenticato turistaTemp2 = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("Leonardo", "rosso", LocalDate.of(2000, Calendar.MARCH, 11), "12Unica@", "user12")), RuoloRegistrazione.CONTRIBUTOR);
        turistaTemp2 = gestorePiattaformaService.cambiaRuolo("admin", turistaTemp2.getUsername(), Ruolo.CURATORE);
        if (!(turistaTemp2 instanceof Curatore curatore))
            throw new IllegalArgumentException("errore");

        TuristaAutenticato turistaTemp3 = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("Fede", "Verde", LocalDate.of(2000, Calendar.MARCH, 11), "13Unica@", "user13")), RuoloRegistrazione.CONTRIBUTOR);
        turistaTemp3 = gestorePiattaformaService.cambiaRuolo("admin", turistaTemp3.getUsername(), Ruolo.ANIMATORE);
        if (!(turistaTemp3 instanceof Animatore animatore))
            throw new IllegalArgumentException("errore");

        TuristaAutenticato turista = gestorePiattaformaService.registra(new ContributorDTO(null, new TuristaAutenticatoDTO("aldo", "neri", LocalDate.of(2002, Calendar.NOVEMBER, 12), "14Unica@", "user14")), RuoloRegistrazione.TURISTA);

        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse("parcheggio centrale", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), new Orario(), TipologiaPuntoInteresse.PARCHEGGIO, contributor.getUsername());
        PuntoInteresse puntoInt2 = poiService.creaPuntoInteresse("parcheggio centrale sotto", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), new Orario(), TipologiaPuntoInteresse.PARCHEGGIO, contributor.getUsername());

        assertEquals(numeroPuntiInteresse, poiService.find(puntoInteresse2 -> puntoInteresse2.getComune().equals(comune)).size());
        curatoreServiceImpl.valutaPuntoInteresse(curatore.getUsername(), puntoInteresse.getId(), Stato.APPROVATO.asBoolean());
        curatoreServiceImpl.valutaPuntoInteresse(curatore.getUsername(), puntoInt2.getId(), Stato.APPROVATO.asBoolean());
        assertEquals(numeroPuntiInteresse + 2, poiService.find(puntoInteresse2 -> puntoInteresse2.getComune().equals(comune)).size());

        turistaAutenticatoService.aggiungiPreferito(turista.getUsername(), puntoInteresse.getId());
        turistaAutenticatoService.aggiungiPreferito(turista.getUsername(), puntoInt2.getId());

        turistaAutenticatoService.aggiungiMateriale(turista.getUsername(), puntoInteresse.getId(), materialeService.crea("/testFoto", TipologiaMateriale.FOTO, turista));

        assertEquals(2, turistaAutenticatoService.findPreferiti(turista.getUsername()).size());
        Set<MaterialeGenerico> materiali = poiService.getMaterialiPoi(puntoInteresse.getId());
        int idMateriale = materiali.stream().toList().getFirst().getId();
        assertEquals(materiali.stream().toList().getFirst(), materialeService.getById(idMateriale).get());
        curatoreServiceImpl.eliminaPuntoInteresse(curatore.getUsername(), puntoInteresse.getId());


        assertEquals(numeroPuntiInteresse + 1, poiService.find(puntoInteresse2 -> puntoInteresse2.getComune().equals(comune)).size());

        assertEquals(1, turistaAutenticatoService.findPreferiti(turista.getUsername()).size());
        assertEquals(Optional.empty(), materialeService.getById(idMateriale));

        assertThrows(IllegalArgumentException.class, () -> poiService.getMaterialiPoi(puntoInteresse.getId()).size());

        PuntoInteresse puntoInteresse1 = poiService.creaPuntoInteresse("parco", new Punto(comune.getPosizione().getLatitudine(), comune.getPosizione().getLongitudine()), new Orario(), TipologiaPuntoInteresse.PARCO, contributor.getUsername());

        int numeroItinerariComune = itinerarioService.find(itinerario -> itinerario.getComune().equals(comune)).size();
        itinerarioService.creaItinerario(curatore.getUsername(), "girodeibar");
        assertEquals(numeroItinerariComune + 1, itinerarioService.find(itinerario -> itinerario.getComune().equals(comune)).size());

        curatoreServiceImpl.valutaPuntoInteresse(curatore.getUsername(), puntoInteresse1.getId(), Stato.APPROVATO.asBoolean());

        Itinerario itinerario2 = itinerarioService.creaItinerario(curatore.getUsername(), "giro dei bar");
        assertEquals(numeroItinerariComune + 2, itinerarioService.find(itinerario -> itinerario.getComune().equals(comune)).size());
        curatoreServiceImpl.eliminaItinerario(curatore.getUsername(), itinerario2.getId());
        assertEquals(numeroItinerariComune + 1, itinerarioService.find(itinerario -> itinerario.getComune().equals(comune)).size());


        int numeroContest = contestService.find(contest -> contest.getCreatore().equals(animatore)).size();
        Contest contest = contestService.creaContest("contest", "spiaggia", animatore, true);
        assertEquals(numeroContest + 1, contestService.find(contest2 -> contest2.getCreatore().equals(animatore)).size());
        contestService.aggiungiMateriale(turista.getUsername(), contest.getId(), materialeService.crea("/testFoto", TipologiaMateriale.FOTO, turista));
        assertEquals(1, contestService.getMaterialiContest(contest).size());

        curatoreServiceImpl.eliminaContest(curatore.getUsername(), contest.getId());

        assertEquals(0, contestService.getMaterialiContest(contest).size());
        assertEquals(numeroContest, contestService.find(contest2 -> contest2.getCreatore().equals(animatore)).size());

        Itinerario itinerario3 = itinerarioService.creaItinerario(curatore.getUsername(), "girodeibar2");
        assertEquals(0, itinerarioService.getTappe(itinerario3.getId()).size());
        itinerarioService.aggiungiTappa(contributor.getUsername(), itinerario3.getId(), puntoInt2.getId());
        assertEquals(1, itinerarioService.getTappe(itinerario3.getId()).size());

        itinerarioService.rimuoviTappa(curatore.getUsername(), itinerario3.getId(), puntoInt2.getId());

        assertEquals(0, itinerarioService.getTappe(itinerario2.getId()).size());


        PuntoInteresse puntoInteresse2 = poiService.creaPuntoInteresse("Castello", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), new Orario(), TipologiaPuntoInteresse.MONUMENTO, contributor.getUsername());

        curatoreServiceImpl.valutaPuntoInteresse(curatore.getUsername(), puntoInteresse2.getId(), Stato.APPROVATO.asBoolean());
        MaterialeGenerico foto = materialeService.crea("/testFoto", TipologiaMateriale.FOTO, turista);
        turistaAutenticatoService.aggiungiMateriale(contributor.getUsername(), puntoInteresse2.getId(), foto);
        curatoreServiceImpl.valutaMateriale(curatore.getUsername(), foto.getId(), Stato.APPROVATO.asBoolean());

        turistaAutenticatoService.aggiungiMateriale(turista.getUsername(), puntoInteresse2.getId(), foto);
        assertEquals(1, poiService.getMaterialiPoi(puntoInteresse2.getId()).size());
        curatoreServiceImpl.eliminaMateriale(curatore.getUsername(), foto.getId());
        assertEquals(0, poiService.getMaterialiPoi(puntoInteresse2.getId()).size());

    }


    /*
     * Test per modificare la scadenza di un contenuto
     */
    @Test
    @Order(6)
    public void modificaScadenzaContenuto() throws ConnessioneFallitaException, FuoriComuneException {
        Comune comune = comuneService.creaComune("Milano", "admin");

        TuristaAutenticato turistaTemp = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 11), "15Unica@", "user15")), RuoloRegistrazione.CONTRIBUTOR);
        if (!(turistaTemp instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");

        int numPoi = poiService.findActive().size();
        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributor.getUsername());
        assertEquals(numPoi + 1, poiService.findActive().size());
        assertNull(puntoInteresse.getExpireDate());


        poiService.modificaScadenza(contributor.getUsername(), puntoInteresse.getId(), LocalDate.of(2025, 2, 1));
        assertEquals(LocalDate.of(2025, 2, 1), poiService.getScadenza(puntoInteresse.getId()));
        assertEquals(numPoi + 1, poiService.findActive().size());
    }
}