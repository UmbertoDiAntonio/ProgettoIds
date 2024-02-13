package ids.unicam.GestioneUtenti;

import ids.unicam.Comune;
import ids.unicam.controller.ComuneController;
import ids.unicam.models.Ruolo;
import ids.unicam.models.Service.*;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.Foto;
import ids.unicam.models.contenuti.POIFactory.AttivitaFactory;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import ids.unicam.utilites.Stato;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.GregorianCalendar;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
public class JUnitUtentiTest {
    private final ComuneController comuneController;
    private final GestioneComuneService gestioneComuneService;
    private final ContributorService contributorService;
    private final ContributorAutorizzatoService contributorAutorizzatoService;
    private final CuratoreService curatoreService;
    private final AnimatoreService animatoreService;
    private final PoiService poiService;
    private final ItinerarioService itinerarioService;
    private final MaterialeService materialeService;
    private final TuristaAutenticatoService turistaAutenticatoService;
    private final GestorePiattaformaService gestorePiattaformaService;
    @Autowired
    public JUnitUtentiTest(ComuneController comuneController, GestioneComuneService gestioneComuneService, ContributorService contributorService, ContributorAutorizzatoService contributorAutorizzatoService, CuratoreService curatoreService, AnimatoreService animatoreService, PoiService poiService, ItinerarioService itinerarioService, MaterialeService materialeService, TuristaAutenticatoService turistaAutenticatoService, GestorePiattaformaService gestorePiattaformaService) {
        this.comuneController = comuneController;
        this.gestioneComuneService = gestioneComuneService;
        this.contributorService = contributorService;
        this.contributorAutorizzatoService = contributorAutorizzatoService;
        this.curatoreService = curatoreService;
        this.animatoreService = animatoreService;
        this.poiService = poiService;
        this.itinerarioService = itinerarioService;
        this.materialeService = materialeService;
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }

    @Test
    public void generazioneUtenti() {

        Comune comune = comuneController.creaComune("nome");

        gestorePiattaformaService.registraTurista("Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "pass", "user");
        gestorePiattaformaService.registraTurista("Paolo", "Giallo", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "pass", "user");
        gestorePiattaformaService.registraContributor(comune, "Giuseppe", "Oro", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "PASS", "user");

        assertEquals(1, gestioneComuneService.getContributorDelComune(comune.getNome()).size());



        gestorePiattaformaService.promuovi(gestioneComuneService.getContributorDelComune(comune.getNome()).getFirst(), Ruolo.Curatore);
        assertEquals(1, gestioneComuneService.getContributorDelComune(comune.getNome()).size());
        assertEquals(1, gestioneComuneService.getCuratoriDelComune(comune.getNome()).size());

        gestorePiattaformaService.promuovi(gestioneComuneService.getCuratoriDelComune(comune.getNome()).getFirst(), Ruolo.Contributor);

        assertEquals(1, gestioneComuneService.getContributorDelComune(comune.getNome()).size());
        assertEquals(0, gestioneComuneService.getCuratoriDelComune(comune.getNome()).size());

        gestorePiattaformaService.promuovi(gestioneComuneService.getContributorDelComune(comune.getNome()).getFirst(), Ruolo.ContributorTrusted);

        assertEquals(1, gestioneComuneService.getContributorAutorizzatiDelComune(comune.getNome()).size());
        assertEquals(0, gestioneComuneService.getCuratoriDelComune(comune.getNome()).size());

    }

    @Test
    public void aggiungiPreferito() {

        Comune comune = comuneController.creaComune("Milano");
        comune.getPosizione();
        Contributor contributor = gestorePiattaformaService.registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "ciao", "mr");
        gestorePiattaformaService.registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "eroe", "AN2");
        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.getTuristi().getLast();
        AttivitaFactory attivitaFactory = new AttivitaFactory();

        gestorePiattaformaService.promuovi(contributor, Ruolo.ContributorTrusted);
        PuntoInteresse puntoInteresse = attivitaFactory.creaPoi(comune,"Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));

        ContributorAutorizzato contributorAutorizzato = gestioneComuneService.getContributorAutorizzatiDelComune(comune.getNome()).getFirst();
        contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato, puntoInteresse);
        assertEquals(0, turistaAutenticato.getPreferiti().size());
        turistaAutenticato.aggiungiPreferito(puntoInteresse);
        assertEquals(1, turistaAutenticato.getPreferiti().size());
    }

    @Test
    public void condividiContenuto() {

        Comune comune = comuneController.creaComune("Milano");
        Contributor contributor = gestorePiattaformaService.registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "ciao", "mr");
        gestorePiattaformaService.promuovi(contributor, Ruolo.Curatore);
        Curatore curatore = gestioneComuneService.getCuratoriDelComune(comune.getNome()).getFirst();
        AttivitaFactory attivitaFactory = new AttivitaFactory();
        PuntoInteresse puntoInteresse = attivitaFactory.creaPoi(comune,"Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));

        assertThrows(UnsupportedOperationException.class, () -> curatoreService.condividi(puntoInteresse));
        //TODO test condivisione contenuto
    }

    @Test
    public void metodoCercaTurista() {
        Turista turista = new Turista();
        /*//TODO fixes
        assertEquals(0,turista.search("empty").size());

        
        Comune comune = comuneController.creaComune("Milano");
        Contributor contributor = gestorePiattaformaService.registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH,17), "ciao", "mr");

        gestorePiattaformaService.promuovi(contributor, Ruolo.Curatore);

        AttivitaFactory attivitaFactory = new AttivitaFactory();
        PuntoInteresse puntoInteresse = attivitaFactory.creaPoi("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015));
        puntoInteresse.aggiungiTag("Edicola");
        contributor.aggiungiPuntoInteresse(puntoInteresse);
        Curatore curatore=gestioneComuneService.getCuratoriDelComune(comune.getNome()).getFirst();
        assertEquals(0,turista.search("Edicola").size());
        curatore.valuta(puntoInteresse, Stato.APPROVED);
        assertEquals(1,turista.search("Edicola").size());


         */
    }

    @Test
    public void aggiungiFoto() {

        Comune comune = comuneController.creaComune("Milano");
        Contributor contributor = gestorePiattaformaService.registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "ciao", "mr");

        contributorService.aggiungiPuntoInteresse(contributor,new AttivitaFactory().creaPoi(comune,"Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015)));

        PuntoInteresse puntoInteresse = gestioneComuneService.getContenuti(comune.getNome()).getFirst();

        gestorePiattaformaService.registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "eroe", "AN2");
        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.getTuristi().getLast();

        assertEquals(0, puntoInteresse.getMateriali().size());
        Foto foto =  new Foto(turistaAutenticato,puntoInteresse);
        //turistaAutenticatoService.aggiungiFoto(puntoInteresse,);
        assertEquals(1, puntoInteresse.getMateriali().size());
        assertFalse(puntoInteresse.getMateriali().getFirst().getStato().asBoolean());
        gestorePiattaformaService.promuovi(contributor, Ruolo.Curatore);
        Curatore curatore = gestioneComuneService.getCuratoriDelComune(comune.getNome()).getFirst();
        curatoreService.valuta(puntoInteresse.getMateriali().getFirst(), Stato.APPROVED);
        assertTrue(puntoInteresse.getMateriali().getFirst().getStato().asBoolean());
    }

}