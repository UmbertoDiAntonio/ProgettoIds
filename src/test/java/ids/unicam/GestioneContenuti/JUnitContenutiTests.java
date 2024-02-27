package ids.unicam.GestioneContenuti;

import ids.unicam.DataBase.GestoreDatabase;
import ids.unicam.Service.impl.*;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.exception.ContestException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Comune;
import ids.unicam.models.DTO.*;
import ids.unicam.models.Invito;
import ids.unicam.models.Punto;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.Foto;
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
    public void testPoi() throws ConnessioneFallitaException, FuoriComuneException {
        Comune comune;
        try {
            comune = comuneService.creaComune(new Comune(new ComuneDTO("Milano")));
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }
        TuristaAutenticato turistaTemp;
        try {
            turistaTemp = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new ComuneDTO(comune.getNome()), new TuristaAutenticatoDTO("Mario", "Rossi", LocalDate.of(2000, 3, 17), "1Unica@", "user1")), Ruolo.CONTRIBUTOR);
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }
        if (!(turistaTemp instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");
        /*
         * Creazione di un Punto di Interesse da parte di un contributor, che di base non è approvato
         */
        {
            int puntiInteresseComuneIniziali = comuneService.getPuntiInteresseNelComune(comune.getNome()).size();
            Orario orario = new Orario();
            orario.setOrarioApertura(DayOfWeek.MONDAY, LocalTime.of(8, 30), LocalTime.of(18, 0));
            PuntoInteresse punto1 = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("bar", new Punto(comune.getPosizione().getLatitudine() + 0.01, comune.getPosizione().getLongitudine() + 0.01), orario, TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributor)));

            assertEquals(puntiInteresseComuneIniziali + 1, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());
            assertNull(punto1.getStato().asBoolean());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted, che di base è approvato
         */
        {
            try {
                gestorePiattaformaService.cambiaRuolo(contributor.getUsername(), Ruolo.CONTRIBUTOR_AUTORIZZATO);
            } catch (ConnessioneFallitaException e) {
                throw new RuntimeException(e);
            }
            ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiDelComune(comune.getNome()).getLast();
            int puntiInteresseComuneIniziali = comuneService.getPuntiInteresseNelComune(comune.getNome()).size();

            PuntoInteresse punto2 = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("bar2", new Punto(comune.getPosizione().getLatitudine() + 0.02, comune.getPosizione().getLongitudine() + 0.02), new Orario(), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributorAutorizzato)));

            assertEquals(puntiInteresseComuneIniziali + 1, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());
            assertEquals(Boolean.TRUE, punto2.getStato().asBoolean());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted, che di base è approvato, ma fuori dall'area del comune
         * quindi non può essere creato
         */
        {
            int puntiInteresseComuneIniziali = comuneService.getPuntiInteresseNelComune(comune.getNome()).size();
            Comune comune2;
            try {
                comune2 = comuneService.creaComune(new Comune(new ComuneDTO("Roma")));
            } catch (ConnessioneFallitaException e) {
                throw new RuntimeException(e);
            }
            TuristaAutenticato turistaTemp2 ;
            try {
                turistaTemp2 = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new ComuneDTO(comune2.getNome()), new TuristaAutenticatoDTO("Mario", "Rossi", LocalDate.of(2000, Calendar.MARCH, 17), "1Unica@", "user19")), Ruolo.CONTRIBUTOR_AUTORIZZATO);
            } catch (ConnessioneFallitaException e) {
                throw new RuntimeException(e);
            }
            if (!(turistaTemp2 instanceof ContributorAutorizzato contributorAutorizzato))
                throw new IllegalArgumentException("errore");

            assertThrows(FuoriComuneException.class, () -> poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("chiesa", new Punto(comune.getPosizione().getLatitudine() + 2, comune.getPosizione().getLongitudine() + 2), new Orario(), TipologiaPuntoInteresse.LUOGO_DI_CULTO, contributorAutorizzato))));
            assertEquals(puntiInteresseComuneIniziali, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted
         * aggiunta di un materiale al punto di interesse
         * verifica finale
         */
        {
            ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiDelComune(comune.getNome()).getFirst();
            PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributorAutorizzato)));

            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registraTurista(new TuristaAutenticatoDTO("andrea", "neri", LocalDate.of(2000, Calendar.FEBRUARY, 3), "2Unica@", "user2"));

            MaterialeGenerico materialeGenerico = materialeService.crea("./fotoTest",TipologiaMateriale.FOTO,turistaAutenticato.getUsername());
            poiService.aggiungiMateriale(contributorAutorizzato.getUsername(), puntoInteresse.getId(), materialeGenerico);

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
            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registraTurista(new TuristaAutenticatoDTO("andrea", "neri", LocalDate.of(2000, Calendar.FEBRUARY, 3), "3Unica@", "user3"));
            TuristaAutenticato turistaTemp1 = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new ComuneDTO(comune.getNome()), new TuristaAutenticatoDTO("Peppe", "Peppe", LocalDate.of(2000, Calendar.MARCH, 11), "4Unica@", "user4")), Ruolo.CURATORE);
            if (!(turistaTemp1 instanceof Curatore curatore))
                throw new IllegalArgumentException("errore");

            Orario orarioAccademia = new Orario();
            orarioAccademia.setOrarioApertura(DayOfWeek.MONDAY, LocalTime.of(9, 0), LocalTime.of(18, 0));
            PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("Accademia", new Punto(comune.getPosizione().getLatitudine() + 0.0, comune.getPosizione().getLongitudine() + 0.0), orarioAccademia, TipologiaPuntoInteresse.CENTRO_SPORTIVO, contributor)));
            assertNull(poiService.getStato(puntoInteresse.getId()).asBoolean());
            TuristaAutenticato turistaTemp2 = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new ComuneDTO(comune.getNome()), new TuristaAutenticatoDTO("Peppe", "Paol", LocalDate.of(2000, Calendar.MARCH, 11), "4Unica@", "user44")), Ruolo.CONTRIBUTOR);

            if (!(turistaTemp2 instanceof Contributor contributor1))
                throw new IllegalArgumentException("errore");
            TuristaAutenticato turistaTemp3 = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new ComuneDTO(comune.getNome()), new TuristaAutenticatoDTO("Pietro", "Pier", LocalDate.of(2000, Calendar.MARCH, 11), "4Unica@", "user45")), Ruolo.CONTRIBUTOR);
            if (!(turistaTemp3 instanceof Contributor contributor2))
                throw new IllegalArgumentException("errore");

            int numeroOsservatori = curatoreServiceImpl.getOsservatori(curatore).size();
            curatoreServiceImpl.aggiungiOsservatore(curatore.getUsername(), contributor1.getUsername());
            assertEquals(numeroOsservatori + 1, curatoreServiceImpl.getNumeroOsservatori(curatore));

            curatoreServiceImpl.aggiungiOsservatore(curatore.getUsername(), contributor2.getUsername());
            assertEquals(numeroOsservatori + 2, curatoreServiceImpl.getNumeroOsservatori(curatore));

            try {
                curatoreServiceImpl.valutaPuntoInteresse(curatore.getUsername(), puntoInteresse.getId(), true);
            } catch (FuoriComuneException e) {
                throw new RuntimeException(e);
            }
            assertThrows(UnsupportedOperationException.class, () -> curatoreServiceImpl.valutaPuntoInteresse(curatore.getUsername(), puntoInteresse.getId(), Stato.NON_APPROVATO.asBoolean()));


            assertEquals(Boolean.TRUE, poiService.getStato(puntoInteresse.getId()).asBoolean());
            MaterialeGenerico materialeGenerico1 = materialeService.save(new Foto(new MaterialeDTO("./testFoto",contributor)));
            assertNull(materialeGenerico1.getStato().asBoolean());
            curatoreServiceImpl.valutaMateriale(curatore.getUsername(), materialeGenerico1.getId(), Stato.APPROVATO.asBoolean());
            poiService.aggiungiMateriale(turistaAutenticato.getUsername(), puntoInteresse.getId(), materialeGenerico1);
            curatoreServiceImpl.rimuoviOsservatore(curatore.getUsername(), contributor1.getUsername());
            assertEquals(numeroOsservatori + 1, curatoreServiceImpl.getNumeroOsservatori(curatore));
        }
    }

    /*
     * Test per le varie possibilità di gestione di un itinerario
     */
    @Test
    @Order(2)
    public void testItinerario() throws ConnessioneFallitaException, FuoriComuneException {
        Comune comune ;
        try {
            comune = comuneService.creaComune(new Comune(new ComuneDTO("Milano")));
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }

        /*
         * Creazione di un itenerario, aggiunta di una tappa e test.
         * Aggiunti di ulteriori tappe e test
         */

        {
            int numeroItinerariIniziale = itinerarioService.findAllByComune(comune).size();
            TuristaAutenticato turistaTemp = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new ComuneDTO(comune.getNome()), new TuristaAutenticatoDTO("Mario", "Rossi", LocalDate.of(2000, Calendar.MARCH, 11), "5Unica@", "user5")), Ruolo.CONTRIBUTOR_AUTORIZZATO);
            if (!(turistaTemp instanceof ContributorAutorizzato contributorAutorizzato))
                throw new IllegalArgumentException("errore");

            PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("farmacia", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), new Orario(), TipologiaPuntoInteresse.SALUTE_E_BENESSERE, contributorAutorizzato)));
            PuntoInteresse puntoInteresse2 = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("centro Commerciale", new Punto(comune.getPosizione().getLatitudine() - 0.02, comune.getPosizione().getLongitudine() - 0.02), new Orario(), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributorAutorizzato)));


            Itinerario itinerario1 = itinerarioService.creaItinerario(contributorAutorizzato.getUsername(), "girodeibar");

            assertEquals(numeroItinerariIniziale + 1, itinerarioService.findAllByComune(comune).size());
            assertEquals(0, itinerarioService.getNumeroTappe(itinerario1));

            PuntoInteresse nuovoPunto1 = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("universita'", new Punto(comune.getPosizione().getLatitudine() + 0.014, comune.getPosizione().getLongitudine() + 0.014), new Orario(), TipologiaPuntoInteresse.FORMAZIONE, contributorAutorizzato)));
            PuntoInteresse nuovoPunto2 = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("universita2'", new Punto(comune.getPosizione().getLatitudine() + 0.014, comune.getPosizione().getLongitudine() + 0.014), new Orario(), TipologiaPuntoInteresse.FORMAZIONE, contributorAutorizzato)));
            PuntoInteresse nuovoPunto3 = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("universita3'", new Punto(comune.getPosizione().getLatitudine() + 0.014, comune.getPosizione().getLongitudine() + 0.014), new Orario(), TipologiaPuntoInteresse.FORMAZIONE, contributorAutorizzato)));


            assertTrue(itinerarioService.aggiungiTappa(contributorAutorizzato.getUsername(), itinerario1.getId(), nuovoPunto1.getId()));
            //itinerarioService.aggiungiTappa(itinerario1, nuovoPunto);

            assertEquals(numeroItinerariIniziale + 1, itinerarioService.findAllByComune(comune).size());
            assertEquals(1, itinerarioService.getNumeroTappe(itinerario1));

            itinerarioService.aggiungiTappa(contributorAutorizzato.getUsername(), itinerario1.getId(), nuovoPunto2.getId(), nuovoPunto3.getId());
            //itinerarioService.aggiungiTappa(itinerario1, nuovoPunto1, nuovoPunto2, nuovoPunto3);
            assertEquals(numeroItinerariIniziale + 1, itinerarioService.findAllByComune(comune).size());

            assertEquals(3, itinerarioService.getNumeroTappe(itinerario1));

        }
    }

    /*
     * Test relativo alla gestione dei Contest
     */

    @Test
    @Order(3)
    public void testContest() throws ConnessioneFallitaException {
        Comune comune ;
        try {
            comune = comuneService.creaComune(new Comune(new ComuneDTO("Milano")));
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }

        TuristaAutenticato turistaTemp = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new ComuneDTO(comune.getNome()), new TuristaAutenticatoDTO("mario", "rossi", LocalDate.of(2000, Calendar.APRIL, 7), "6Unica@", "user6")), Ruolo.ANIMATORE);
        if (!(turistaTemp instanceof Animatore animatore))
            throw new IllegalArgumentException("errore");
        int numeroContestCreatiDaAnimatore = contestService.getContestByCreatore(animatore).size();
        /*
         * Creazione di un nuovo contesto libero e successivo join del turista al contest
         * aggiunta di un materiale al contest dal turistaLoggato
         */
        {

            Contest contest = contestService.creaContest(new Contest(new RichiestaCreazioneContestDTO("Monumento", "Foto più bella", animatore, true)));

            assertEquals(numeroContestCreatiDaAnimatore + 1, contestService.getContestByCreatore(animatore).size());

            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registraTurista(new TuristaAutenticatoDTO("andrea", "neri", LocalDate.of(2000, Calendar.MARCH, 11), "7Unica@", "user7"));

            turistaAutenticatoService.partecipaAlContest(contest.getId(), turistaAutenticato.getUsername());

            try {
                contestService.aggiungiMateriale(materialeService.crea("./testFoto",TipologiaMateriale.FOTO,turistaAutenticato.getUsername()), contest, turistaAutenticato);
            } catch (ContestException e) {
                throw new RuntimeException(e);
            }


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
            Contest contest = contestService.creaContest(new Contest(new RichiestaCreazioneContestDTO("Monumento", "Foto più bella", animatore, false)));
            assertEquals(numeroContestCreatiDaAnimatore + 2, contestService.getContestByCreatore(animatore).size());

            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registraTurista(new TuristaAutenticatoDTO("andrea", "neri", LocalDate.of(2000, Calendar.NOVEMBER, 5), "8Unica@", "user8"));
            Invito invito ;
            try {
                invito = animatoreServiceImpl.invitaContest(animatore.getUsername(), contest.getId(), turistaAutenticato.getUsername());
            } catch (ContestException e) {
                throw new RuntimeException(e);
            }

            assertTrue(invitoService.isValid(new InvitoDTO(invito.getContest(), invito.getInvitato())));

            turistaAutenticatoService.accettaInvitoContest(new TuristaAutenticatoDTO(
                    turistaAutenticato.getNome(),
                    turistaAutenticato.getCognome(),
                    turistaAutenticato.getDataNascita(),
                    turistaAutenticato.getPassword(),
                    turistaAutenticato.getUsername()), new InvitoDTO(invito.getContest(), invito.getInvitato()));

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
    public void approvaMaterialeByAnimatore() throws ConnessioneFallitaException {
        Comune comune ;
        try {
            comune = comuneService.creaComune(new Comune(new ComuneDTO("Milano")));
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }
        TuristaAutenticato turistaTemp = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new ComuneDTO(comune.getNome()), new TuristaAutenticatoDTO("mario", "rossi", LocalDate.of(2000, Calendar.OCTOBER, 1), "9Unica@", "user9")), Ruolo.ANIMATORE);

        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registraTurista(new TuristaAutenticatoDTO("andrea", "neri", LocalDate.of(2000, Calendar.DECEMBER, 3), "10Unica@", "user10"));

        if (!(turistaTemp instanceof Animatore animatore))
            throw new IllegalArgumentException("errore");


        Contest contest = contestService.creaContest(new Contest(new RichiestaCreazioneContestDTO("Monumento", "Foto più bella", animatore, true)));

        MaterialeGenerico descrizione = materialeService.crea("./testFoto",TipologiaMateriale.FOTO,turistaAutenticato.getUsername());
        assertThrows(ContestException.class, () -> contestService.aggiungiMateriale(descrizione, contest, turistaAutenticato));

        turistaAutenticatoService.partecipaAlContest(contest.getId(), turistaAutenticato.getUsername());
        MaterialeGenerico materialeGenerico = materialeService.crea("./testFoto",TipologiaMateriale.FOTO,turistaAutenticato.getUsername());
        try {
            contestService.aggiungiMateriale(descrizione, contest, turistaAutenticato);
        } catch (ContestException e) {
            throw new RuntimeException(e);
        }
        assertNull(materialeGenerico.getStato().asBoolean());
        assertEquals(1, contestService.getMaterialiContest(contest).size());

        assertTrue(animatoreServiceImpl.approvaMateriale(animatore.getUsername(), contest.getId(), materialeGenerico.getId(), true));

        assertEquals(Boolean.TRUE, materialeService.getStato(materialeGenerico).asBoolean());

    }

    /*
     * Test relativo all'eliminazione di un contenuto, che può essere un POI, un materiale, un itinerario e un contest
     */
    @Test
    @Order(5)
    public void eliminaContenuto() throws ConnessioneFallitaException, FuoriComuneException {

        Comune comune;
        try {
            comune = comuneService.creaComune(new Comune(new ComuneDTO("Milano")));
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }
        int numeroPuntiInteresse = comuneService.getPuntiInteresseNelComune(comune.getNome()).size();
        TuristaAutenticato turistaTemp = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new ComuneDTO(comune.getNome()), new TuristaAutenticatoDTO("mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 5), "11Unica@", "user11")), Ruolo.ANIMATORE);
        if (!(turistaTemp instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");

        TuristaAutenticato turistaTemp2 = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new ComuneDTO(comune.getNome()), new TuristaAutenticatoDTO("Leonardo", "rosso", LocalDate.of(2000, Calendar.MARCH, 11), "12Unica@", "user12")), Ruolo.CURATORE);
        if (!(turistaTemp2 instanceof Curatore curatore))
            throw new IllegalArgumentException("errore");

        TuristaAutenticato turistaTemp3 = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new ComuneDTO(comune.getNome()), new TuristaAutenticatoDTO("Fede", "Verde", LocalDate.of(2000, Calendar.MARCH, 11), "13Unica@", "user13")), Ruolo.ANIMATORE);
        if (!(turistaTemp3 instanceof Animatore animatore))
            throw new IllegalArgumentException("errore");

        TuristaAutenticato turista = gestorePiattaformaService.registraTurista(new TuristaAutenticatoDTO("aldo", "neri", LocalDate.of(2002, Calendar.NOVEMBER, 12), "14Unica@", "user14"));

        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("parcheggio centrale", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), new Orario(), TipologiaPuntoInteresse.PARCHEGGIO, contributor)));
        PuntoInteresse puntoInt2 = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("parcheggio centrale sotto", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), new Orario(), TipologiaPuntoInteresse.PARCHEGGIO, contributor)));

        assertEquals(numeroPuntiInteresse + 2, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());
        puntoInteresse=curatoreServiceImpl.valutaPuntoInteresse(curatore.getUsername(), puntoInteresse.getId(), Stato.APPROVATO.asBoolean());
        puntoInt2=curatoreServiceImpl.valutaPuntoInteresse(curatore.getUsername(), puntoInt2.getId(), Stato.APPROVATO.asBoolean());

        turistaAutenticatoService.aggiungiPreferito(turista.getUsername(), puntoInteresse);
        turistaAutenticatoService.aggiungiPreferito(turista.getUsername(), puntoInt2);

        assertEquals(2, turistaAutenticatoService.findPreferiti(turista.getUsername()).size());

        curatoreServiceImpl.eliminaPuntoInteresse(curatore.getUsername(), puntoInteresse.getId());

        assertEquals(numeroPuntiInteresse + 1, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());

        assertEquals(1, turistaAutenticatoService.findPreferiti(turista.getUsername()).size());

        PuntoInteresse puntoInteresse1 = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("parco", new Punto(comune.getPosizione().getLatitudine(), comune.getPosizione().getLongitudine()), new Orario(), TipologiaPuntoInteresse.PARCO, contributor)));

        int numeroItinerariComune = itinerarioService.findAllByComune(comune).size();
        itinerarioService.creaItinerario(curatore.getUsername(), "girodeibar");
        assertEquals(numeroItinerariComune+1, itinerarioService.findAllByComune(comune).size());

        curatoreServiceImpl.valutaPuntoInteresse(curatore.getUsername(), puntoInteresse1.getId(), Stato.APPROVATO.asBoolean());

        Itinerario itinerario2 = itinerarioService.creaItinerario(curatore.getUsername(), "giro dei bar");
        assertEquals(numeroItinerariComune + 2, itinerarioService.findAllByComune(comune).size());
        curatoreServiceImpl.eliminaItinerario(curatore.getUsername(), itinerario2.getId());
        assertEquals(numeroItinerariComune +1, itinerarioService.findAllByComune(comune).size());


        int numeroContest = contestService.getContestByCreatore(animatore).size();
        Contest contest = contestService.creaContest(new Contest(new RichiestaCreazioneContestDTO("contest", "spiaggia", animatore, true)));
        assertEquals(numeroContest + 1, contestService.getContestByCreatore(animatore).size());
        curatoreServiceImpl.eliminaContest(curatore.getUsername(), contest.getId());
        assertEquals(numeroContest, contestService.getContestByCreatore(animatore).size());

        Itinerario itinerario3 = itinerarioService.creaItinerario(curatore.getUsername(), "girodeibar2");
        assertEquals(0, itinerarioService.getNumeroTappe(itinerario3));
        assertTrue(itinerarioService.aggiungiTappa(contributor.getUsername(), itinerario3.getId(), puntoInt2.getId()));
        assertEquals(1, itinerarioService.getNumeroTappe(itinerario3));

        itinerarioService.rimuoviTappa(curatore.getUsername(), itinerario3.getId(), puntoInt2.getId());

        assertEquals(0, itinerarioService.getNumeroTappe(itinerario2));


        PuntoInteresse puntoInteresse2 = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("Castello", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), new Orario(), TipologiaPuntoInteresse.MONUMENTO, contributor)));

        curatoreServiceImpl.valutaPuntoInteresse(curatore.getUsername(), puntoInteresse2.getId(), Stato.APPROVATO.asBoolean());
        MaterialeGenerico foto = materialeService.crea("./testFoto",TipologiaMateriale.FOTO,turista.getUsername());
        curatoreServiceImpl.valutaMateriale(curatore.getUsername(), foto.getId(), Stato.APPROVATO.asBoolean());

        poiService.aggiungiMateriale(turista.getUsername(), puntoInteresse2.getId(), foto);
        assertEquals(1, poiService.getMaterialiPoi(puntoInteresse2.getId()).size());
        curatoreServiceImpl.elimina(curatore, foto);
        assertEquals(0, poiService.getMaterialiPoi(puntoInteresse2.getId()).size());

    }


    /*
     * Test per modificare la scadenza di un contentuto
     */
    @Test
    @Order(6)
    public void modificaScadenzaContenuto() throws ConnessioneFallitaException, FuoriComuneException {
        Comune comune ;
        try {
            comune = comuneService.creaComune(new Comune(new ComuneDTO("Milano")));
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }
        TuristaAutenticato turistaTemp = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new ComuneDTO(comune.getNome()), new TuristaAutenticatoDTO("mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 11), "15Unica@", "user15")), Ruolo.CONTRIBUTOR);
        if (!(turistaTemp instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");

        int numPoi = poiService.findActive().size();
        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributor)));
        assertEquals(numPoi + 1, poiService.findActive().size());
        assertEquals(LocalDate.MAX, puntoInteresse.getExpireDate());


        poiService.modificaScadenza(contributor.getUsername(), puntoInteresse.getId(), LocalDate.of(2024, 2, 1));
        assertEquals(LocalDate.of(2024, 2, 1), poiService.getScadenza(puntoInteresse.getId()));
        assertEquals(numPoi, poiService.findActive().size());
    }
}