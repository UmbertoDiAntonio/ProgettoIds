package ids.unicam.GestioneUtenti;

import ids.unicam.DataBase.GestoreDatabase;
import ids.unicam.Service.impl.*;
import ids.unicam.models.Comune;
import ids.unicam.models.Punto;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.Foto;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import ids.unicam.models.contenuti.puntiInteresse.TipologiaPuntoInteresse;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.GregorianCalendar;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
public class JUnitUtentiTest {

    private final ComuneServiceImpl comuneService;
    private final ContributorServiceImpl contributorService;
    private final ContributorAutorizzatoServiceImpl contributorAutorizzatoServiceImpl;
    private final CuratoreServiceImpl curatoreServiceImpl;
    private final PoiServiceImpl poiService;
    private final MaterialeServiceImpl materialeService;
    private final TuristaAutenticatoServiceImpl turistaAutenticatoService;
    private final GestorePiattaformaServiceImpl gestorePiattaformaService;
    private final TuristaServiceImpl turistaService;


    @Autowired
    public JUnitUtentiTest(ComuneServiceImpl comuneService, ContributorServiceImpl contributorService, ContributorAutorizzatoServiceImpl contributorAutorizzatoServiceImpl, CuratoreServiceImpl curatoreServiceImpl, PoiServiceImpl poiService, MaterialeServiceImpl materialeService, TuristaAutenticatoServiceImpl turistaAutenticatoService, GestorePiattaformaServiceImpl gestorePiattaformaService, TuristaServiceImpl turistaService, GestoreDatabase gestoreDatabase) {
        this.comuneService = comuneService;
        this.contributorService = contributorService;
        this.contributorAutorizzatoServiceImpl = contributorAutorizzatoServiceImpl;
        this.curatoreServiceImpl = curatoreServiceImpl;
        this.poiService = poiService;
        this.materialeService = materialeService;
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.gestorePiattaformaService = gestorePiattaformaService;
        this.turistaService = turistaService;
        gestoreDatabase.eliminaTabelleDB();
        gestoreDatabase.inizializzaDatabase();
    }

    @Test
    public void generazioneUtenti() {

        Comune comune = comuneService.creaComune("nome");

        int numeroContributor = comuneService.getContributorDelComune(comune.getNome()).size();
        int numeroContributorAutorizzati = comuneService.getContributorAutorizzatiDelComune(comune.getNome()).size();
        int numeroCuratori = comuneService.getCuratoriDelComune(comune.getNome()).size();

        gestorePiattaformaService.registra(null, Ruolo.TURISTA, "Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "1Unico@", "user1");
        gestorePiattaformaService.registra(null, Ruolo.TURISTA, "Paolo", "Giallo", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "2Unico@", "user2");
        gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR, "Giuseppe", "Oro", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "3Unico@", "user3");

        assertEquals(numeroContributor + 1, comuneService.getContributorDelComune(comune.getNome()).size());


        gestorePiattaformaService.cambiaRuolo(comuneService.getContributorDelComune(comune.getNome()).getFirst(), Ruolo.CURATORE);
        assertEquals(numeroContributor + 1, comuneService.getContributorDelComune(comune.getNome()).size());
        assertEquals(numeroCuratori + 1, comuneService.getCuratoriDelComune(comune.getNome()).size());

        gestorePiattaformaService.cambiaRuolo(comuneService.getCuratoriDelComune(comune.getNome()).getFirst(), Ruolo.CONTRIBUTOR);

        assertEquals(numeroContributor + 1, comuneService.getContributorDelComune(comune.getNome()).size());
        assertEquals(numeroCuratori, comuneService.getCuratoriDelComune(comune.getNome()).size());

        gestorePiattaformaService.cambiaRuolo(comuneService.getContributorDelComune(comune.getNome()).getFirst(), Ruolo.CONTRIBUTOR_AUTORIZZATO);

        assertEquals(numeroContributorAutorizzati + 1, comuneService.getContributorAutorizzatiDelComune(comune.getNome()).size());
        assertEquals(numeroCuratori, comuneService.getCuratoriDelComune(comune.getNome()).size());

    }

    @Test
    public void aggiungiPreferito() {

        Comune comune = comuneService.creaComune("Milano");

        TuristaAutenticato turista = gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "4Unico@", "user4");
        if(!(turista instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");
        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(null, Ruolo.TURISTA, "andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "5Unico@", "user5");

        gestorePiattaformaService.cambiaRuolo(contributor, Ruolo.CONTRIBUTOR_AUTORIZZATO);
        PuntoInteresse puntoInteresse = new PuntoInteresse(comune, "Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE);

        ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiDelComune(comune.getNome()).getFirst();
        contributorAutorizzatoServiceImpl.aggiungiPuntoInteresse(contributorAutorizzato, puntoInteresse);
        assertEquals(0, turistaAutenticato.getPreferiti().size());
        turistaAutenticatoService.aggiungiPreferito(turistaAutenticato, puntoInteresse);
        assertEquals(1, turistaAutenticato.getPreferiti().size());
    }

    @Test
    public void condividiContenuto() {

        Comune comune = comuneService.creaComune("Milano");
        TuristaAutenticato turista = gestorePiattaformaService.registra(comune, Ruolo.CURATORE, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "6Unico@", "user6");
        if(!(turista instanceof Curatore curatore))
            throw new IllegalArgumentException("errore");
        PuntoInteresse puntoInteresse = new PuntoInteresse(comune, "Teatro", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), TipologiaPuntoInteresse.INTRATTENIMENTO);

        assertThrows(UnsupportedOperationException.class, () -> curatoreServiceImpl.condividi(puntoInteresse));
        //TODO test condivisione contenuto
    }

    @Test
    public void metodoCercaTurista() {

        Comune comune = comuneService.creaComune("Milano");
        TuristaAutenticato turista1 = gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "7Unico@", "user7");
        if(!(turista1 instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");


        TuristaAutenticato turista2 = gestorePiattaformaService.registra(comune, Ruolo.CURATORE, "mirco", "blu", new GregorianCalendar(2002, GregorianCalendar.MAY, 15), "8Unico@", "user8");
        if(!(turista2 instanceof Curatore curatore))
            throw new IllegalArgumentException("errore");

        PuntoInteresse puntoInteresse = new PuntoInteresse(comune, "Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE);


        int numeroTagEdicolaIniziale = turistaService.findByTag(new Tag("Edicola",puntoInteresse)).size();

        contributorService.aggiungiPuntoInteresse(contributor, puntoInteresse);

        poiService.aggiungiTag(puntoInteresse, new Tag("Edicola",puntoInteresse));
        assertEquals(numeroTagEdicolaIniziale + 1, turistaService.findByTag(new Tag("Edicola",puntoInteresse)).size());

        curatoreServiceImpl.valuta(curatore,puntoInteresse, Stato.APPROVATO);


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

        TuristaAutenticato turista = gestorePiattaformaService.registra(comune, Ruolo.CONTRIBUTOR, "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "9Unico@", "user9");
        if(!(turista instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");

        PuntoInteresse puntoInteresse = contributorService.aggiungiPuntoInteresse(contributor, new PuntoInteresse(comune, "parco centrale", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), TipologiaPuntoInteresse.PARCO));
        gestorePiattaformaService.cambiaRuolo(contributor, Ruolo.CURATORE);
        Curatore curatore = comuneService.getCuratoriDelComune(comune.getNome()).getLast();
        curatoreServiceImpl.valuta(curatore,puntoInteresse, Stato.APPROVATO);
        assertTrue(puntoInteresse.getStato().asBoolean());
        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(null, Ruolo.TURISTA, "andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "10Unico@", "user10");


        assertEquals(0, poiService.getMaterialiPoi(puntoInteresse).size());
        MaterialeGenerico foto = new Foto(turistaAutenticato);
        poiService.aggiungiMateriale(turistaAutenticato, puntoInteresse, foto);
        assertEquals(1, poiService.getMaterialiPoi(puntoInteresse).size());
        assertFalse(foto.getStato().asBoolean());
        curatoreServiceImpl.valuta(curatore,foto, Stato.APPROVATO);
        assertTrue(foto.getStato().asBoolean());


    }

}