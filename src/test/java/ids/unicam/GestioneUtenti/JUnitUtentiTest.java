package ids.unicam.GestioneUtenti;

import ids.unicam.DataBase.GestoreDatabase;
import ids.unicam.Service.impl.*;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Comune;
import ids.unicam.models.DTO.ContributorDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.Punto;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.RuoloRegistrazione;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.materiali.TipologiaMateriale;
import ids.unicam.models.contenuti.puntiInteresse.Orario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.TipologiaPuntoInteresse;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.time.LocalDate;
import java.util.Calendar;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
public class JUnitUtentiTest {
    private final ComuneServiceImpl comuneService;
    private final CuratoreServiceImpl curatoreServiceImpl;
    private final PoiServiceImpl poiService;
    private final TuristaAutenticatoServiceImpl turistaAutenticatoService;
    private final GestorePiattaformaServiceImpl gestorePiattaformaService;
    private final TuristaServiceImpl turistaService;
    private final MaterialeServiceImpl materialeService;


    @Autowired
    public JUnitUtentiTest(ComuneServiceImpl comuneService, CuratoreServiceImpl curatoreServiceImpl, PoiServiceImpl poiService, TuristaAutenticatoServiceImpl turistaAutenticatoService, GestorePiattaformaServiceImpl gestorePiattaformaService, TuristaServiceImpl turistaService, GestoreDatabase gestoreDatabase, MaterialeServiceImpl materialeService) {
        this.comuneService = comuneService;
        this.curatoreServiceImpl = curatoreServiceImpl;
        this.poiService = poiService;
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.gestorePiattaformaService = gestorePiattaformaService;
        this.turistaService = turistaService;
        this.materialeService = materialeService;
        gestoreDatabase.eliminaTabelleDB();
        gestoreDatabase.inizializzaDatabase();
    }

    @Test
    public void generazioneUtenti() throws ConnessioneFallitaException {
        Comune comune = comuneService.creaComune("nome", "admin");


        int numeroContributor = comuneService.getContributorDelComune(comune.getNome(), "admin").size();
        int numeroContributorAutorizzati = comuneService.getContributorAutorizzatiDelComune(comune.getNome(), "admin").size();
        int numeroCuratori = comuneService.getCuratoriDelComune(comune.getNome(), "admin").size();

        gestorePiattaformaService.registra(new ContributorDTO(null, new TuristaAutenticatoDTO("Mario", "Rossi", LocalDate.of(2000, Calendar.MARCH, 17), "1Unico@", "user1")), RuoloRegistrazione.TURISTA);
        gestorePiattaformaService.registra(new ContributorDTO(null, new TuristaAutenticatoDTO("Paolo", "Giallo", LocalDate.of(2000, Calendar.MARCH, 17), "2Unico@", "user2")), RuoloRegistrazione.TURISTA);
        gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("Giuseppe", "Oro", LocalDate.of(2000, Calendar.MARCH, 17), "3Unico@", "user3")), RuoloRegistrazione.CONTRIBUTOR);

        assertEquals(numeroContributor + 1, comuneService.getContributorDelComune(comune.getNome(), "admin").size());

        Contributor contributor = comuneService.getContributorDelComune(comune.getNome(), "admin").getLast();


        gestorePiattaformaService.cambiaRuolo("admin", contributor.getUsername(), Ruolo.CURATORE);

        assertEquals(numeroContributor + 1, comuneService.getContributorDelComune(comune.getNome(), "admin").size());
        assertEquals(numeroCuratori + 1, comuneService.getCuratoriDelComune(comune.getNome(), "admin").size());

        Curatore curatore = comuneService.getCuratoriDelComune(comune.getNome(), "admin").getLast();

        gestorePiattaformaService.cambiaRuolo("admin", curatore.getUsername(), Ruolo.CONTRIBUTOR);


        assertEquals(numeroContributor + 1, comuneService.getContributorDelComune(comune.getNome(), "admin").size());
        assertEquals(numeroCuratori, comuneService.getCuratoriDelComune(comune.getNome(), "admin").size());

        Contributor contributor1 = comuneService.getContributorDelComune(comune.getNome(), "admin").getLast();

        try {
            gestorePiattaformaService.cambiaRuolo("admin", contributor1.getUsername(), Ruolo.CONTRIBUTOR_AUTORIZZATO);
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }

        assertEquals(numeroContributorAutorizzati + 1, comuneService.getContributorAutorizzatiDelComune(comune.getNome(), "admin").size());
        assertEquals(numeroCuratori, comuneService.getCuratoriDelComune(comune.getNome(), "admin").size());

    }

    @Test
    public void aggiungiPreferito() throws ConnessioneFallitaException, FuoriComuneException {

        Comune comune = comuneService.creaComune("Milano", "admin");


        TuristaAutenticato turista = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 17), "4Unico@", "user4")), RuoloRegistrazione.CONTRIBUTOR);
        if (!(turista instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");
        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(new ContributorDTO(null, new TuristaAutenticatoDTO("andrea", "neri", LocalDate.of(2000, Calendar.MARCH, 17), "5Unico@", "user5")), RuoloRegistrazione.TURISTA);

        try {
            gestorePiattaformaService.cambiaRuolo("admin", contributor.getUsername(), Ruolo.CONTRIBUTOR_AUTORIZZATO);
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }
        ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiDelComune(comune.getNome(), "admin").getFirst();
        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributorAutorizzato.getUsername());
        assertEquals(0, turistaAutenticato.getPreferiti().size());

        turistaAutenticatoService.aggiungiPreferito(turistaAutenticato.getUsername(), puntoInteresse);
        assertEquals(1, turistaAutenticatoService.findPreferiti(turistaAutenticato.getUsername()).size());
    }


    @Test
    public void metodoCercaTurista() throws ConnessioneFallitaException, FuoriComuneException {
        Comune comune = comuneService.creaComune("Milano", "admin");
        TuristaAutenticato turista1 = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 17), "7Unico@", "user7")), RuoloRegistrazione.CONTRIBUTOR);
        if (!(turista1 instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");


        TuristaAutenticato turista2 = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("mirco", "blu", LocalDate.of(2002, Calendar.MAY, 15), "8Unico@", "user8")), RuoloRegistrazione.CONTRIBUTOR);
        turista2 = gestorePiattaformaService.cambiaRuolo("admin", turista2.getUsername(), Ruolo.CURATORE);
        if (!(turista2 instanceof Curatore curatore))
            throw new IllegalArgumentException("errore");

        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributor.getUsername());


        int numeroTagEdicolaIniziale = turistaService.findByTag("Edicola").size();


        poiService.aggiungiTag(puntoInteresse.getId(), "Edicola", contributor.getUsername());
        assertEquals(numeroTagEdicolaIniziale + 1, turistaService.findByTag("Edicola").size());

        curatoreServiceImpl.valutaPuntoInteresse(curatore.getUsername(), puntoInteresse.getId(), Stato.APPROVATO.asBoolean());


        poiService.aggiungiTag(puntoInteresse.getId(), "Tabaccheria", contributor.getUsername());

        assertEquals(numeroTagEdicolaIniziale + 1, turistaService.findByTag("Edicola").size());

        assertEquals("Edicola", poiService.getTags(puntoInteresse).getFirst());
        assertEquals("Tabaccheria", poiService.getTags(puntoInteresse).getLast());


        poiService.aggiungiTag(puntoInteresse.getId(), "Bar", contributor.getUsername());

        assertEquals(numeroTagEdicolaIniziale + 1, turistaService.findByTag("Edicola").size());

        assertTrue(poiService.getTags(puntoInteresse).contains("Edicola"));
        assertTrue(poiService.getTags(puntoInteresse).contains("Tabaccheria"));
        assertTrue(poiService.getTags(puntoInteresse).contains("Bar"));

    }

    @Test
    public void aggiungiFoto() throws ConnessioneFallitaException, FuoriComuneException {
        Comune comune = comuneService.creaComune("Milano", "admin");

        TuristaAutenticato turista = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 17), "9Unico@", "user9")), RuoloRegistrazione.CONTRIBUTOR);
        if (!(turista instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");

        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse("parco centrale", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.PARCO, contributor.getUsername());

        TuristaAutenticato turista2 = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 17), "9Unico@", "user99")), RuoloRegistrazione.CONTRIBUTOR);
        turista2 = gestorePiattaformaService.cambiaRuolo("admin", turista2.getUsername(), Ruolo.CURATORE);
        if (!(turista2 instanceof Curatore curatore1))
            throw new IllegalArgumentException("errore");

        curatoreServiceImpl.valutaPuntoInteresse(curatore1.getUsername(), puntoInteresse.getId(), Stato.APPROVATO.asBoolean());
        assertEquals(Boolean.TRUE, poiService.getStato(puntoInteresse.getId()).asBoolean());
        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registra(new ContributorDTO(null, new TuristaAutenticatoDTO("andrea", "neri", LocalDate.of(2000, Calendar.MARCH, 17), "10Unico@", "user10")), RuoloRegistrazione.TURISTA);


        assertEquals(0, poiService.getMaterialiPoi(puntoInteresse.getId()).size());
        MaterialeGenerico foto = materialeService.crea("/testFoto.jpg", TipologiaMateriale.FOTO, turistaAutenticato);
        foto.getBase64();
        poiService.aggiungiMateriale(turistaAutenticato.getUsername(), puntoInteresse.getId(), foto);
        assertEquals(1, poiService.getMaterialiPoi(puntoInteresse.getId()).size());
        assertNull(materialeService.getStato(foto).get().asBoolean());
        curatoreServiceImpl.valutaMateriale(curatore1.getUsername(), foto.getId(), Stato.APPROVATO.asBoolean());
        assertEquals(Boolean.TRUE, materialeService.getStato(foto).get().asBoolean());
    }

    @Test
    public void segnalaContenuto() throws ConnessioneFallitaException, FuoriComuneException {
        Comune comune = comuneService.creaComune("Milano", "admin");

        TuristaAutenticato turista = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 17), "9Unico@", "user91")), RuoloRegistrazione.CONTRIBUTOR);
        if (!(turista instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");

        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse("n0me b4rutt0 ", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.PARCO, contributor.getUsername());


        TuristaAutenticato turista2 = gestorePiattaformaService.registra(new ContributorDTO(comune, new TuristaAutenticatoDTO("mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 17), "9Unico@", "user92")), RuoloRegistrazione.CONTRIBUTOR);
        turista2 = gestorePiattaformaService.cambiaRuolo("admin", turista2.getUsername(), Ruolo.CURATORE);
        if (!(turista2 instanceof Curatore curatore))
            throw new ClassCastException("Non è possibile trasformare il turista " + turista2 + " in un Curatore");
        assertEquals(0, turistaAutenticatoService.visualizzaNotifiche(curatore.getUsername()).size());
        turistaService.report(puntoInteresse.getId(), "Nome Sbagliato");

        assertEquals(1, turistaAutenticatoService.visualizzaNotifiche(curatore.getUsername()).size());


    }


}