package ids.unicam.GestioneUtenti;

import ids.unicam.DataBase.GestoreDatabase;
import ids.unicam.Service.impl.*;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Comune;
import ids.unicam.models.DTO.*;
import ids.unicam.models.Punto;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.Foto;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.materiali.TipologiaMateriale;
import ids.unicam.models.contenuti.puntiInteresse.Orario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
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

        Comune comune = null;
        try {
            comune = comuneService.creaComune(new Comune(new RichiestaCreazioneComuneDTO("nome")));
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }

        int numeroContributor = comuneService.getContributorDelComune(comune.getNome()).size();
        int numeroContributorAutorizzati = comuneService.getContributorAutorizzatiDelComune(comune.getNome()).size();
        int numeroCuratori = comuneService.getCuratoriDelComune(comune.getNome()).size();

        gestorePiattaformaService.registraTurista(new TuristaAutenticatoDTO( "Mario", "Rossi", LocalDate.of(2000, Calendar.MARCH, 17), "1Unico@", "user1"));
        gestorePiattaformaService.registraTurista(new TuristaAutenticatoDTO( "Paolo", "Giallo", LocalDate.of(2000, Calendar.MARCH, 17), "2Unico@", "user2"));
        gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new RichiestaCreazioneComuneDTO(comune.getNome()),new TuristaAutenticatoDTO("Giuseppe", "Oro", LocalDate.of(2000, Calendar.MARCH, 17), "3Unico@", "user3")),Ruolo.CONTRIBUTOR);

        assertEquals(numeroContributor + 1, comuneService.getContributorDelComune(comune.getNome()).size());

        Contributor contributor = comuneService.getContributorDelComune(comune.getNome()).getLast();

        try {
            gestorePiattaformaService.cambiaRuolo(contributor.getUsername(), Ruolo.CURATORE);
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }
        assertEquals(numeroContributor + 1, comuneService.getContributorDelComune(comune.getNome()).size());
        assertEquals(numeroCuratori + 1, comuneService.getCuratoriDelComune(comune.getNome()).size());

        Curatore curatore = comuneService.getCuratoriDelComune(comune.getNome()).getLast();

        try {
            gestorePiattaformaService.cambiaRuolo(curatore.getUsername(),Ruolo.CONTRIBUTOR);
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }

        assertEquals(numeroContributor + 1, comuneService.getContributorDelComune(comune.getNome()).size());
        assertEquals(numeroCuratori, comuneService.getCuratoriDelComune(comune.getNome()).size());

        Contributor contributor1 = comuneService.getContributorDelComune(comune.getNome()).getLast();

        try {
            gestorePiattaformaService.cambiaRuolo(contributor1.getUsername(), Ruolo.CONTRIBUTOR_AUTORIZZATO);
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }

        assertEquals(numeroContributorAutorizzati + 1, comuneService.getContributorAutorizzatiDelComune(comune.getNome()).size());
        assertEquals(numeroCuratori, comuneService.getCuratoriDelComune(comune.getNome()).size());

    }

    @Test
    public void aggiungiPreferito() throws ConnessioneFallitaException, FuoriComuneException {

        Comune comune = null;
        try {
            comune = comuneService.creaComune(new Comune(new RichiestaCreazioneComuneDTO("Milano")));
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }

        TuristaAutenticato turista = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new RichiestaCreazioneComuneDTO(comune.getNome()),new TuristaAutenticatoDTO( "mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 17), "4Unico@", "user4")),Ruolo.CONTRIBUTOR);
        if (!(turista instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");
        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registraTurista(new TuristaAutenticatoDTO( "andrea", "neri", LocalDate.of(2000, Calendar.MARCH, 17), "5Unico@", "user5"));

        try {
            gestorePiattaformaService.cambiaRuolo(contributor.getUsername(), Ruolo.CONTRIBUTOR_AUTORIZZATO);
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }
        ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiDelComune(comune.getNome()).getFirst();
        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributorAutorizzato)));
        assertEquals(0, turistaAutenticato.getPreferiti().size());

        turistaAutenticatoService.aggiungiPreferito(turistaAutenticato.getUsername(), puntoInteresse.getId());
        assertEquals(1, turistaAutenticatoService.findPreferiti(turistaAutenticato.getUsername()).size());
    }

    @Test
    public void condividiContenuto() throws ConnessioneFallitaException, FuoriComuneException {

        Comune comune = null;
        try {
            comune = comuneService.creaComune(new Comune(new RichiestaCreazioneComuneDTO("Milano")));
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }
        TuristaAutenticato turista = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new RichiestaCreazioneComuneDTO(comune.getNome()),new TuristaAutenticatoDTO( "mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 17), "6Unico@", "user6")),Ruolo.CURATORE);
        if (!(turista instanceof Curatore curatore))
            throw new IllegalArgumentException("errore");
        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("Teatro", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.INTRATTENIMENTO, curatore)));

        assertThrows(UnsupportedOperationException.class, () -> {
            curatoreServiceImpl.condividi(curatore.getUsername(), puntoInteresse.getId());
        });
        //TODO test condivisione contenuto
    }

    @Test
    public void metodoCercaTurista() throws ConnessioneFallitaException, FuoriComuneException {

        Comune comune = null;
        try {
            comune = comuneService.creaComune(new Comune(new RichiestaCreazioneComuneDTO("Milano")));
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }
        TuristaAutenticato turista1 = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new RichiestaCreazioneComuneDTO(comune.getNome()),new TuristaAutenticatoDTO( "mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 17), "7Unico@", "user7")),Ruolo.CONTRIBUTOR);
        if (!(turista1 instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");


        TuristaAutenticato turista2 = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new RichiestaCreazioneComuneDTO(comune.getNome()),new TuristaAutenticatoDTO( "mirco", "blu", LocalDate.of(2002, Calendar.MAY, 15), "8Unico@", "user8")),Ruolo.CURATORE);
        if (!(turista2 instanceof Curatore curatore))
            throw new IllegalArgumentException("errore");

        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributor)));


        int numeroTagEdicolaIniziale = turistaService.findByTag("Edicola").size();


        poiService.aggiungiTag(puntoInteresse, new Tag("Edicola"));
        assertEquals(numeroTagEdicolaIniziale + 1, turistaService.findByTag("Edicola").size());

        curatoreServiceImpl.valutaPuntoInteresse(curatore.getUsername(), puntoInteresse.getId(), Stato.APPROVATO.asBoolean());


        poiService.aggiungiTag(puntoInteresse, new Tag("Tabaccheria"));

        assertEquals(numeroTagEdicolaIniziale + 1, turistaService.findByTag("Edicola").size());

        assertEquals("Edicola", poiService.getTags(puntoInteresse).getFirst().getValore());
        assertEquals("Tabaccheria", poiService.getTags(puntoInteresse).getLast().getValore());


        poiService.aggiungiTag(puntoInteresse, new Tag("Bar"));

        assertEquals(numeroTagEdicolaIniziale + 1, turistaService.findByTag("Edicola").size());

        assertEquals("Edicola", poiService.getTags(puntoInteresse).getFirst().getValore());
        assertEquals("Tabaccheria", poiService.getTags(puntoInteresse).get(1).getValore());
        assertEquals("Bar", poiService.getTags(puntoInteresse).getLast().getValore());

    }

    @Test
    public void aggiungiFoto() throws ConnessioneFallitaException, FuoriComuneException {
        Comune comune = null;
        try {
            comune = comuneService.creaComune(new Comune(new RichiestaCreazioneComuneDTO("Milano")));
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }

        TuristaAutenticato turista = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new RichiestaCreazioneComuneDTO(comune.getNome()),new TuristaAutenticatoDTO( "mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 17), "9Unico@", "user9")),Ruolo.CONTRIBUTOR);
        if (!(turista instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");

        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("parco centrale", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.PARCO, contributor)));

        TuristaAutenticato turist2 = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new RichiestaCreazioneComuneDTO(comune.getNome()),new TuristaAutenticatoDTO( "mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 17), "9Unico@", "user99")),Ruolo.CURATORE);
        if (!(turist2 instanceof Curatore curatore1))
            throw new IllegalArgumentException("errore");

        curatoreServiceImpl.valutaPuntoInteresse(curatore1.getUsername(), puntoInteresse.getId(), Stato.APPROVATO.asBoolean());
        assertEquals(Boolean.TRUE, poiService.getStato(puntoInteresse.getId()).asBoolean());
        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registraTurista(new TuristaAutenticatoDTO( "andrea", "neri", LocalDate.of(2000, Calendar.MARCH, 17), "10Unico@", "user10"));


        assertEquals(0, poiService.getMaterialiPoi(puntoInteresse.getId()).size());
        MaterialeGenerico foto = materialeService.crea("./testFoto", TipologiaMateriale.FOTO,turistaAutenticato.getUsername());
        poiService.aggiungiMateriale(turistaAutenticato.getUsername(), puntoInteresse.getId(), foto);
        assertEquals(1, poiService.getMaterialiPoi(puntoInteresse.getId()).size());
        assertNull(materialeService.getStato(foto).asBoolean());
        curatoreServiceImpl.valutaMateriale(curatore1.getUsername(), foto.getId(), Stato.APPROVATO.asBoolean());
        assertEquals(Boolean.TRUE, materialeService.getStato(foto).asBoolean());
    }

    @Test
    public void segnalaContenuto() throws ConnessioneFallitaException, FuoriComuneException {
        Comune comune = null;
        try {
            comune = comuneService.creaComune(new Comune(new RichiestaCreazioneComuneDTO("Milano")));
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }

        TuristaAutenticato turista = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new RichiestaCreazioneComuneDTO(comune.getNome()),new TuristaAutenticatoDTO( "mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 17), "9Unico@", "user91")),Ruolo.CONTRIBUTOR);
        if (!(turista instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");

        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("n0me b4rutt0 ", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.PARCO, contributor)));


        TuristaAutenticato turista2 = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(new RichiestaCreazioneComuneDTO(comune.getNome()),new TuristaAutenticatoDTO( "mario", "rossi", LocalDate.of(2000, Calendar.MARCH, 17), "9Unico@", "user92")),Ruolo.CURATORE);
        if (!(turista2 instanceof Curatore curatore))
            throw new ClassCastException("Non è possibile trasformare il turista " + turista2 + " in un Curatore");
        assertEquals(0, turistaAutenticatoService.visualizzaNotifiche(curatore.getUsername()).size());
        turistaService.report(new PuntoInteresseDTO(puntoInteresse.getNome(),puntoInteresse.getPt(),puntoInteresse.getOrario(), puntoInteresse.getTipo(), puntoInteresse.getCreatore()), "Nome Sbagliato");

        assertEquals(1, turistaAutenticatoService.visualizzaNotifiche(curatore.getUsername()).size());


    }


}