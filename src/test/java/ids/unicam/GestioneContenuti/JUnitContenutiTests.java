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


    private final ComuneController comuneController;
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

    @Autowired
    public JUnitContenutiTests(ComuneController comuneController, ComuneService comuneService, ContributorService contributorService, ContributorAutorizzatoService contributorAutorizzatoService, CuratoreService curatoreService, AnimatoreService animatoreService, TuristaAutenticatoService turistaAutenticatoService, PoiService poiService, ItinerarioService itinerarioService, MaterialeService materialeService, ContestService contestService, GestorePiattaformaService gestorePiattaformaService) {
        this.comuneController = comuneController;
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
        Comune comune = comuneController.creaComune("Milano");
        Contributor contributor = gestorePiattaformaService.registraContributor(comune, "Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "pass", "user");

        /*
         * Creazione di un Punto di Interesse da parte di un contributor, che di base non è approvato
         */
        {
            int puntiInteresseComuneIniziali = comuneService.getPuntiInteresseNelComune(comune.getNome()).size();
            Orario orario = new Orario();
            orario.setOrarioApertura(DayOfWeek.MONDAY,LocalTime.of(8,30),LocalTime.of(18,0));
            PuntoInteresse punto1 = new PuntoInteresse(comune,"bar", new Punto(comune.getPosizione().getLatitudine() + 0.01, comune.getPosizione().getLongitudine() + 0.01), orario, TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE);
            contributorService.aggiungiPuntoInteresse(contributor,punto1);//TODO check se si mette
            

            assertEquals(puntiInteresseComuneIniziali + 1, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());
            assertFalse(comuneService.getPuntiInteresseNelComune(comune.getNome()).getLast().getStato().asBoolean());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted, che di base è approvato
         */
        {
            gestorePiattaformaService.promuovi(comuneService.getContributorByComune(comune.getNome()).getLast(), Ruolo.ContributorTrusted);

            ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiByComune(comune.getNome()).getLast();

            assertEquals(1, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());
            PuntoInteresse punto2 = (new PuntoInteresse(comune,"bar2", new Punto(comune.getPosizione().getLatitudine() + 0.02, comune.getPosizione().getLongitudine() + 0.02),TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE));

            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato,punto2);//TODO check e true
            //il problema qui è avere 2 istanze diverse del Controller
            assertEquals(2, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());
            assertTrue(comuneService.getPuntiInteresseNelComune(comune.getNome()).getLast().getStato().asBoolean());
        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted, che di base è approvato, ma fuori dall'area del comune
         * quindi non può essere creato
         */
        {
            ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiByComune(comune.getNome()).getFirst();
            PuntoInteresse punto3 = new PuntoInteresse(comune,"chiesa", new Punto(comune.getPosizione().getLatitudine() + 2, comune.getPosizione().getLongitudine() + 2),TipologiaPuntoInteresse.LUOGO_DI_CULTO);
            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato,punto3);//TODO check se true

        }

        /*
         * Creazione di un Punto di Interesse da parte di un contributorTrusted
         * aggiunta di un materiale al punto di interesse
         * verifica finale
         */
        {
            ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiByComune(comune.getNome()).getFirst();
            PuntoInteresse puntoInteresse = new PuntoInteresse(comune,"Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015),TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE);
            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato,puntoInteresse);
            gestorePiattaformaService.registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.FEBRUARY, 3), "eroe", "AN2");
            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.getTuristi().getLast();
            MaterialeGenerico materialeGenerico = new Foto(turistaAutenticato,puntoInteresse);
            contributorAutorizzatoService.aggiungiMateriale(contributorAutorizzato, puntoInteresse, materialeGenerico);
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
            gestorePiattaformaService.registraContributor(comune, "Peppe", "Peppe", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "PASS", "user");
            gestorePiattaformaService.promuovi(comuneService.getContributorByComune(comune.getNome()).getFirst(), Ruolo.Curatore);
            Curatore curatore = comuneService.getCuratoriByComune(comune.getNome()).getFirst();

            Orario orarioAccademia = new Orario();
            orarioAccademia.setOrarioApertura(DayOfWeek.MONDAY, LocalTime.of(9, 0), LocalTime.of(18, 0));
            contributorService.aggiungiPuntoInteresse(contributor,new PuntoInteresse(comune,"Accademia", new Punto(comune.getPosizione().getLatitudine() + 0.01, comune.getPosizione().getLongitudine() + 0.01), orarioAccademia,TipologiaPuntoInteresse.CENTRO_SPORTIVO));
            assertFalse(comuneService.getPuntiInteresseNelComune(comune.getNome()).getLast().getStato().asBoolean());
            curatore.aggiungiOsservatore(contributor);
            assertEquals(1, curatore.getOsservatori().size());
            
            curatoreService.valuta(comuneService.getPuntiInteresseNelComune(comune.getNome()).getLast(), Stato.APPROVED);
            assertTrue(comuneService.getPuntiInteresseNelComune(comune.getNome()).getLast().getStato().asBoolean());
            MaterialeGenerico materialeGenerico1 = new Foto(contributor,comuneService.getPuntiInteresseNelComune(comune.getNome()).getLast());
            assertFalse(materialeService.findByWhere(comuneService.getPuntiInteresseNelComune(comune.getNome()).getLast()).getLast().getStato().asBoolean());
            curatoreService.valuta(materialeGenerico1, Stato.APPROVED);
            assertTrue(materialeService.findByWhere(comuneService.getPuntiInteresseNelComune(comune.getNome()).getLast()).getLast().getStato().asBoolean());
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

        Comune comune = comuneController.creaComune("Milano");

        /*
         * Creazione di un itenerario, aggiunta di una tappa e test.
         * Aggiunti di ulteriori tappe e test
         */

        {
            gestorePiattaformaService.registraContributor(comune, "Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "pass", "user");
            gestorePiattaformaService.promuovi(comuneService.getContributorByComune(comune.getNome()).getFirst(), Ruolo.ContributorTrusted);

            ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiByComune(comune.getNome()).getFirst();
            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato,new PuntoInteresse(comune,"farmacia", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), TipologiaPuntoInteresse.SALUTE_E_BENESSERE));
            contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato,new PuntoInteresse(comune,"centro Commerciale", new Punto(comune.getPosizione().getLatitudine() - 0.02, comune.getPosizione().getLongitudine() - 0.02), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE));
            Itinerario itinerario1 = contributorAutorizzatoService.aggiungiItinerario(new Itinerario(comune,"girodeibar", comuneService.getPuntiInteresseNelComune(comune.getNome()).get(0), comuneService.getPuntiInteresseNelComune(comune.getNome()).get(1)));
            
            assertEquals(1, itinerarioService.findAllByComune(comune).size());
            assertEquals(2, itinerarioService.findAllByComune(comune).getFirst().getNumeroTappe());

            PuntoInteresse nuovoPunto = new PuntoInteresse(comune,"universita'", new Punto(comune.getPosizione().getLatitudine() + 0.14, comune.getPosizione().getLongitudine() + 0.14),TipologiaPuntoInteresse.FORMAZIONE);
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

        Comune comune = comuneController.creaComune("Milano");
        Contributor contributor = gestorePiattaformaService.registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.APRIL, 7), "ciao", "mr");
        gestorePiattaformaService.promuovi(contributor, Ruolo.Animatore);
        Animatore animatore = comuneService.getAnimatoriByComune(comune.getNome()).getFirst();


        /*
         * Creazione di un nuovo contesto libero e successivo join del turista al contest
         * aggiunta di un materiale al contest dal turistaLoggato
         */

        {

            Contest contest=animatoreService.creaContest(animatore,"Monumento", "Foto più bella", true);
            assertEquals(1,contestService.getContestByCreatore(animatore).size());

            gestorePiattaformaService.registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "eroe", "AN2");
            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.getTuristi().getLast();
            MaterialeGenerico materialeGenerico =materialeService.save( turistaAutenticato, new Foto(turistaAutenticato,contest));

            turistaAutenticato.partecipaAlContest(contestService.getContestByCreatore(animatore).getLast());
            assertEquals(1, contestService.getContestByCreatore(animatore).getLast().getPartecipanti().size());
            assertEquals(1,contestService.getMaterialiContest(contest).size());
        }

        /*
         * Creazione di un contest privato, quindi su invito.
         * Invio dell'invito da parte dell'animatore
         * Verifica della validità dell'invito
         * Accettazione invito da parte del turista
         */

        {
            Contest contest=animatoreService.creaContest(animatore, "Contest","Foto più bella", false);
            assertEquals(2, contestService.getContestByCreatore(animatore).size());

            gestorePiattaformaService.registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.NOVEMBER, 5), "eroe", "AN2");
            TuristaAutenticato turistaAutenticato = gestorePiattaformaService.getTuristi().getLast();
            animatoreService.invitaContest(animatore,contest,turistaAutenticato);

            assertTrue(turistaAutenticato.getInvitiRicevuti().getFirst().isValid());
            turistaAutenticatoService.accettaInvitoContest(turistaAutenticato,turistaAutenticato.getInvitiRicevuti().getFirst());
            assertEquals(1, contestService.getContestByCreatore(animatore).getLast().getPartecipanti().size());
            assertEquals(contestService.getContestByCreatore(animatore).getLast(), contestService.getContestByPartecipante(turistaAutenticato).getLast());
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

        Comune comune = comuneController.creaComune("Milano");
        Contributor contributor = gestorePiattaformaService.registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.OCTOBER, 1), "ciao", "mr");
        gestorePiattaformaService.registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.DECEMBER, 3), "eroe", "AN2");
        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.getTuristi().getLast();

        gestorePiattaformaService.promuovi(contributor, Ruolo.Animatore);
        Animatore animatore = comuneService.getAnimatoriByComune(comune.getNome()).getFirst();
        Contest contest = animatoreService.creaContest(animatore,"monumento", "Foto più bella", true);
        MaterialeGenerico materialeGenerico = new Descrizione(turistaAutenticato,contest);
        //TODO assertThrows(ContestException.class, () -> turistaAutenticatoService.aggiungiMaterialeContest(contest,turistaAutenticato, materialeGenerico));
        turistaAutenticato.partecipaAlContest(contest);
        assertFalse(materialeGenerico.getStato().asBoolean());
        assertEquals(1, contestService.getMaterialiContest(contest).size());
        animatoreService.approvaMateriale(animatore,contest,materialeGenerico,Stato.APPROVED);
        assertTrue(materialeGenerico.getStato().asBoolean());

    }

    /*
     * Test relativo all'eliminazione di un contenuto, che può essere un POI, un materiale, un itinerario e un contest
     */
    @Test
    @Order(5)
    public void eliminaContenuto() {

        Comune comune = comuneController.creaComune("Milano");
        Contributor contributor = gestorePiattaformaService.registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 5), "ciao", "mr");
        gestorePiattaformaService.registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "eroe", "AN2");
        Contributor contributor2 = gestorePiattaformaService.registraContributor(comune, "Leonardo", "rosso", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "esc", "org");
        Contributor contributor3 = gestorePiattaformaService.registraContributor(comune, "Fede", "Verde", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "arg", "use");

        gestorePiattaformaService.promuovi(contributor2, Ruolo.Curatore);
        gestorePiattaformaService.promuovi(contributor3, Ruolo.Animatore);
        Animatore animatore = comuneService.getAnimatoriByComune(comune.getNome()).getFirst();
        Curatore curatore = comuneService.getCuratoriByComune(comune.getNome()).getFirst();

        gestorePiattaformaService.registraTurista("aldo", "neri", new GregorianCalendar(2002, 7, 12), "password", "user1234");
        TuristaAutenticato turista = gestorePiattaformaService.getTuristi().getLast();


        contributorService.aggiungiPuntoInteresse(contributor,new PuntoInteresse(comune,"parcheggio centrale", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), TipologiaPuntoInteresse.PARCHEGGIO));

        assertEquals(1, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());
        curatoreService.valuta(comuneService.getPuntiInteresseNelComune(comune.getNome()).getLast(), Stato.APPROVED);
        turista.aggiungiPreferito(comuneService.getPuntiInteresseNelComune(comune.getNome()).getLast());
        assertEquals(1, turista.getPreferiti().size());
        curatoreService.elimina(comuneService.getPuntiInteresseNelComune(comune.getNome()).getLast());

        assertEquals(0, comuneService.getPuntiInteresseNelComune(comune.getNome()).size());
        //assertEquals(0, turista.getPreferiti().size());//TODO

        contributorService.aggiungiPuntoInteresse(contributor,new PuntoInteresse(comune,"parco", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03), TipologiaPuntoInteresse.PARCO));

        
        contributorService.aggiungiItinerario(new Itinerario(comune,"girodeibar", comuneService.getPuntiInteresseNelComune(comune.getNome()).getFirst()));
        assertEquals(0, itinerarioService.findAllByComune(comune).size());
        curatoreService.valuta(comuneService.getPuntiInteresseNelComune(comune.getNome()).getFirst(), Stato.toStatus(true));
        Itinerario itinerario2 = contributorService.aggiungiItinerario(new Itinerario(comune,"giro dei bar", comuneService.getPuntiInteresseNelComune(comune.getNome()).getFirst()));
        assertEquals(1, itinerarioService.findAllByComune(comune).size());
        curatoreService.elimina(itinerario2);
        assertEquals(0, itinerarioService.findAllByComune(comune).size());

        animatoreService.creaContest(animatore,"contest", "spiaggia", true);
        assertEquals(1, contestService.getContestByCreatore(animatore).size());
        curatoreService.elimina(contestService.getContestByCreatore(animatore).getLast());
        assertEquals(0, contestService.getContestByCreatore(animatore).size());

        Itinerario itinerario3 = contributorService.aggiungiItinerario(new Itinerario(comune,"girodeibar2", comuneService.getPuntiInteresseNelComune(comune.getNome()).getFirst()));
        assertEquals(1, itinerarioService.findAllByComune(comune).getFirst().getNumeroTappe());
        assertTrue(contributorService.aggiungiTappaItinerario(itinerario3, comuneService.getPuntiInteresseNelComune(comune.getNome()).getFirst()));
        assertEquals(2, itinerarioService.findAllByComune(comune).getFirst().getNumeroTappe());
        curatoreService.rimuoviTappa(itinerario3, comuneService.getPuntiInteresseNelComune(comune.getNome()).getFirst());
        assertEquals(1, itinerarioService.findAllByComune(comune).getFirst().getNumeroTappe());

        Foto foto = new Foto(turista,comuneService.getPuntiInteresseNelComune(comune.getNome()).getLast());

        contributorService.aggiungiPuntoInteresse(contributor,new PuntoInteresse(comune,"Castello", new Punto(comune.getPosizione().getLatitudine() + 0.03, comune.getPosizione().getLongitudine() + 0.03),TipologiaPuntoInteresse.MONUMENTO));
        curatoreService.valuta(foto, Stato.toStatus(true));
        assertEquals(1, materialeService.findByWhere(comuneService.getPuntiInteresseNelComune(comune.getNome()).getLast()).size());
        curatoreService.elimina(foto);
        assertEquals(0, materialeService.findByWhere(comuneService.getPuntiInteresseNelComune(comune.getNome()).getLast()).size());


        //TODO aggiungere eliminazione dai preferiti
    }

    /*
     * Test per modificare la scadenza di un contentuto
     */

    @Test
    @Order(6)
    public void modificaScadenzaContenuto() {

        Comune comune = comuneController.creaComune("Milano");
        Contributor contributor = gestorePiattaformaService.registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 11), "ciao", "mr");

        PuntoInteresse puntoInteresse = new PuntoInteresse(comune,"Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE);

        contributorService.aggiungiPuntoInteresse(contributor,puntoInteresse);

        assertThrows(UnsupportedOperationException.class, () -> contributor.aggiungiScadenzaContenuto(puntoInteresse, new Tempo()));
        //TODO test modifica scadenza
    }
}