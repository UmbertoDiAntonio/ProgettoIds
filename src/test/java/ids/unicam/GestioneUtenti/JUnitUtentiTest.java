package ids.unicam.GestioneUtenti;

import ids.unicam.Comune;
import ids.unicam.models.Ruolo;
import ids.unicam.models.Service.*;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.*;
import ids.unicam.utilites.Punto;
import ids.unicam.utilites.Stato;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.GregorianCalendar;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
public class JUnitUtentiTest {
    private final ComuneService comuneService;
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
    private final TuristaService turistaService;

    @Autowired
    public JUnitUtentiTest(ComuneService comuneService, GestioneComuneService gestioneComuneService, ContributorService contributorService, ContributorAutorizzatoService contributorAutorizzatoService, CuratoreService curatoreService, AnimatoreService animatoreService, PoiService poiService, ItinerarioService itinerarioService, MaterialeService materialeService, TuristaAutenticatoService turistaAutenticatoService, GestorePiattaformaService gestorePiattaformaService, TuristaService turistaService) {
        this.comuneService = comuneService;
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
        this.turistaService = turistaService;
    }

    @Test
    public void generazioneUtenti() {

        Comune comune = comuneService.creaComune("nome");

        int numeroContributor = gestioneComuneService.getContributorDelComune(comune.getNome()).size();
        int numeroContributorAutorizzati = gestioneComuneService.getContributorAutorizzatiDelComune(comune.getNome()).size();
        int numeroCuratori = gestioneComuneService.getCuratoriDelComune(comune.getNome()).size();

        gestorePiattaformaService.registraTurista("Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "pass", "user");
        gestorePiattaformaService.registraTurista("Paolo", "Giallo", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "pass", "user");
        gestorePiattaformaService.registraContributor(comune, "Giuseppe", "Oro", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "PASS", "user");

        assertEquals(numeroContributor + 1, gestioneComuneService.getContributorDelComune(comune.getNome()).size());


        gestorePiattaformaService.promuovi(gestioneComuneService.getContributorDelComune(comune.getNome()).getFirst(), Ruolo.Curatore);
        assertEquals(numeroContributor + 1, gestioneComuneService.getContributorDelComune(comune.getNome()).size());
        assertEquals(numeroCuratori + 1, gestioneComuneService.getCuratoriDelComune(comune.getNome()).size());

        gestorePiattaformaService.promuovi(gestioneComuneService.getCuratoriDelComune(comune.getNome()).getFirst(), Ruolo.Contributor);

        assertEquals(numeroContributor + 1, gestioneComuneService.getContributorDelComune(comune.getNome()).size());
        assertEquals(numeroCuratori, gestioneComuneService.getCuratoriDelComune(comune.getNome()).size());

        gestorePiattaformaService.promuovi(gestioneComuneService.getContributorDelComune(comune.getNome()).getFirst(), Ruolo.ContributorTrusted);

        assertEquals(numeroContributorAutorizzati + 1, gestioneComuneService.getContributorAutorizzatiDelComune(comune.getNome()).size());
        assertEquals(numeroCuratori, gestioneComuneService.getCuratoriDelComune(comune.getNome()).size());

    }

    @Test
    public void aggiungiPreferito() {

        Comune comune = comuneService.creaComune("Milano");

        Contributor contributor = gestorePiattaformaService.registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "ciao", "mr");
        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "eroe", "AN2");

        gestorePiattaformaService.promuovi(contributor, Ruolo.ContributorTrusted);
        PuntoInteresse puntoInteresse = new PuntoInteresse(comune, "Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE);

        ContributorAutorizzato contributorAutorizzato = gestioneComuneService.getContributorAutorizzatiDelComune(comune.getNome()).getFirst();
        contributorAutorizzatoService.aggiungiPuntoInteresse(contributorAutorizzato, puntoInteresse);
        assertEquals(0, turistaAutenticato.getPreferiti().size());
        turistaAutenticatoService.aggiungiPreferito(turistaAutenticato, puntoInteresse);
        assertEquals(1, turistaAutenticato.getPreferiti().size());
    }

    @Test
    public void condividiContenuto() {

        Comune comune = comuneService.creaComune("Milano");
        Contributor contributor = gestorePiattaformaService.registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "ciao", "mr");
        gestorePiattaformaService.promuovi(contributor, Ruolo.Curatore);
        Curatore curatore = gestioneComuneService.getCuratoriDelComune(comune.getNome()).getFirst();
        PuntoInteresse puntoInteresse = new PuntoInteresse(comune, "Teatro", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), TipologiaPuntoInteresse.INTRATTENIMENTO);

        assertThrows(UnsupportedOperationException.class, () -> curatoreService.condividi(puntoInteresse));
        //TODO test condivisione contenuto
    }

    @Test
    public void metodoCercaTurista() {
        Turista turista = new Turista(); //TODO, il cerca è stato dato al poiService, ma potrebbe dover essere di un Turista

        Comune comune = comuneService.creaComune("Milano");
        Contributor contributor = gestorePiattaformaService.registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "ciao", "mr");
        Contributor contributor2 = gestorePiattaformaService.registraContributor(comune, "mirco", "blu", new GregorianCalendar(2002, GregorianCalendar.MAY, 15), "ciao", "mr");

        gestorePiattaformaService.promuovi(contributor2, Ruolo.Curatore);
        Curatore curatore = comuneService.getCuratoriByComune(comune.getNome()).getLast();

        PuntoInteresse puntoInteresse = new PuntoInteresse(comune, "Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE);


        int numeroTagEdicolaIniziale = turistaService.findByTag(new Tag("Edicola",puntoInteresse)).size();

        contributorService.aggiungiPuntoInteresse(contributor, puntoInteresse);

        poiService.aggiungiTag(puntoInteresse, new Tag("Edicola",puntoInteresse));
        assertEquals(numeroTagEdicolaIniziale + 1, turistaService.findByTag(new Tag("Edicola",puntoInteresse)).size());

        curatoreService.valuta(puntoInteresse, Stato.APPROVED);


        poiService.aggiungiTag(puntoInteresse, new Tag("Tabaccheria",puntoInteresse));

        assertEquals(numeroTagEdicolaIniziale + 1, turistaService.findByTag(new Tag("Edicola",puntoInteresse)).size());

        assertEquals("Edicola", poiService.getTags(puntoInteresse).getFirst().getValore());
        assertEquals("Tabaccheria", poiService.getTags(puntoInteresse).getLast().getValore());


        poiService.aggiungiTag(puntoInteresse, new Tag("Bar",puntoInteresse));

        assertEquals(numeroTagEdicolaIniziale + 1, turistaService.findByTag(new Tag("Edicola",puntoInteresse)).size());

        assertEquals("Edicola", poiService.getTags(puntoInteresse).getFirst().getValore());
        assertEquals("Tabaccheria", poiService.getTags(puntoInteresse).get(1).getValore());
        assertEquals("Bar", poiService.getTags(puntoInteresse).getLast().getValore());

    }

    @Test
    public void aggiungiFoto() {
        Comune comune = comuneService.creaComune("Milano");
        Contributor contributor = gestorePiattaformaService.registraContributor(comune, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "ciao", "mr");

        PuntoInteresse puntoInteresse = contributorService.aggiungiPuntoInteresse(contributor, new PuntoInteresse(comune, "parco centrale", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), TipologiaPuntoInteresse.PARCO));
        gestorePiattaformaService.promuovi(contributor, Ruolo.Curatore);
        Curatore curatore = gestioneComuneService.getCuratoriDelComune(comune.getNome()).getLast();
        curatoreService.valuta(puntoInteresse, Stato.APPROVED);
        assertTrue(puntoInteresse.getStato().asBoolean());
        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registraTurista("andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "eroe", "AN2");


        assertEquals(0, materialeService.findByWhere(puntoInteresse).size());
        MaterialeGenerico foto = poiService.creaMateriale(turistaAutenticato, puntoInteresse, new Foto(turistaAutenticato));
        assertEquals(1, materialeService.findByWhere(puntoInteresse).size());
        assertFalse(foto.getStato().asBoolean());
        curatoreService.valuta(foto, Stato.APPROVED);
        assertTrue(foto.getStato().asBoolean());


    }

}