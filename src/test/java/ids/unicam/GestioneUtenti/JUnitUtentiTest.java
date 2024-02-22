package ids.unicam.GestioneUtenti;

import ids.unicam.DataBase.GestoreDatabase;
import ids.unicam.Service.impl.*;
import ids.unicam.models.Comune;
import ids.unicam.models.DTO.*;
import ids.unicam.models.Punto;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.Foto;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.Orario;
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
    private final CuratoreServiceImpl curatoreServiceImpl;
    private final PoiServiceImpl poiService;
    private final TuristaAutenticatoServiceImpl turistaAutenticatoService;
    private final GestorePiattaformaServiceImpl gestorePiattaformaService;
    private final TuristaServiceImpl turistaService;


    @Autowired
    public JUnitUtentiTest(ComuneServiceImpl comuneService, CuratoreServiceImpl curatoreServiceImpl, PoiServiceImpl poiService, TuristaAutenticatoServiceImpl turistaAutenticatoService, GestorePiattaformaServiceImpl gestorePiattaformaService, TuristaServiceImpl turistaService, GestoreDatabase gestoreDatabase) {
        this.comuneService = comuneService;
        this.curatoreServiceImpl = curatoreServiceImpl;
        this.poiService = poiService;
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.gestorePiattaformaService = gestorePiattaformaService;
        this.turistaService = turistaService;
        gestoreDatabase.eliminaTabelleDB();
        gestoreDatabase.inizializzaDatabase();
    }

    @Test
    public void generazioneUtenti() {

        Comune comune = comuneService.creaComune(new Comune(new RichiestaCreazioneComuneDTO("nome")));

        int numeroContributor = comuneService.getContributorDelComune(comune.getNome()).size();
        int numeroContributorAutorizzati = comuneService.getContributorAutorizzatiDelComune(comune.getNome()).size();
        int numeroCuratori = comuneService.getCuratoriDelComune(comune.getNome()).size();

        gestorePiattaformaService.registraTurista(new TuristaAutenticatoDTO( "Mario", "Rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "1Unico@", "user1"));
        gestorePiattaformaService.registraTurista(new TuristaAutenticatoDTO( "Paolo", "Giallo", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "2Unico@", "user2"));
        gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(comune,new TuristaAutenticatoDTO("Giuseppe", "Oro", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "3Unico@", "user3"),Ruolo.CONTRIBUTOR));

        assertEquals(numeroContributor + 1, comuneService.getContributorDelComune(comune.getNome()).size());

        Contributor contributor = comuneService.getContributorDelComune(comune.getNome()).getLast();

        gestorePiattaformaService.cambiaRuolo(new RichiestaCreazioneContributorDTO(contributor.getComune(), new TuristaAutenticatoDTO(contributor.getNome(),contributor.getCognome(),contributor.getDataNascita(),contributor.getPassword(), contributor.getUsername()),Ruolo.CONTRIBUTOR), Ruolo.CURATORE);
        assertEquals(numeroContributor + 1, comuneService.getContributorDelComune(comune.getNome()).size());
        assertEquals(numeroCuratori + 1, comuneService.getCuratoriDelComune(comune.getNome()).size());

        Curatore curatore = comuneService.getCuratoriDelComune(comune.getNome()).getLast();

        gestorePiattaformaService.cambiaRuolo(new RichiestaCreazioneContributorDTO(curatore.getComune(), new TuristaAutenticatoDTO(curatore.getNome(),curatore.getCognome(),curatore.getDataNascita(),curatore.getPassword(), curatore.getUsername()),Ruolo.CURATORE),Ruolo.CONTRIBUTOR);

        assertEquals(numeroContributor + 1, comuneService.getContributorDelComune(comune.getNome()).size());
        assertEquals(numeroCuratori, comuneService.getCuratoriDelComune(comune.getNome()).size());

        Contributor contributor1 = comuneService.getContributorDelComune(comune.getNome()).getLast();

        gestorePiattaformaService.cambiaRuolo(new RichiestaCreazioneContributorDTO(contributor1.getComune(), new TuristaAutenticatoDTO(contributor1.getNome(),contributor1.getCognome(),contributor1.getDataNascita(),contributor1.getPassword(), contributor1.getUsername()),Ruolo.CONTRIBUTOR), Ruolo.CONTRIBUTOR_AUTORIZZATO);

        assertEquals(numeroContributorAutorizzati + 1, comuneService.getContributorAutorizzatiDelComune(comune.getNome()).size());
        assertEquals(numeroCuratori, comuneService.getCuratoriDelComune(comune.getNome()).size());

    }

    @Test
    public void aggiungiPreferito() {

        Comune comune = comuneService.creaComune(new Comune(new RichiestaCreazioneComuneDTO("Milano")));

        TuristaAutenticato turista = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(comune,new TuristaAutenticatoDTO( "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "4Unico@", "user4"),Ruolo.CONTRIBUTOR));
        if (!(turista instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");
        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registraTurista(new TuristaAutenticatoDTO( "andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "5Unico@", "user5"));

        gestorePiattaformaService.cambiaRuolo(new RichiestaCreazioneContributorDTO(contributor.getComune(), new TuristaAutenticatoDTO(contributor.getNome(),contributor.getCognome(),contributor.getDataNascita(),contributor.getPassword(), contributor.getUsername()),Ruolo.CONTRIBUTOR), Ruolo.CONTRIBUTOR_AUTORIZZATO);
        ContributorAutorizzato contributorAutorizzato = comuneService.getContributorAutorizzatiDelComune(comune.getNome()).getFirst();
        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributorAutorizzato)));
        assertEquals(0, turistaAutenticato.getPreferiti().size());

        turistaAutenticatoService.aggiungiPreferito(turistaAutenticato.getUsername(), puntoInteresse.getId());
        assertEquals(1, turistaAutenticatoService.findPreferiti(turistaAutenticato.getUsername()).size());
    }

    @Test
    public void condividiContenuto() {

        Comune comune = comuneService.creaComune(new Comune(new RichiestaCreazioneComuneDTO("Milano")));
        TuristaAutenticato turista = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(comune,new TuristaAutenticatoDTO( "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "6Unico@", "user6"),Ruolo.CURATORE));
        if (!(turista instanceof Curatore curatore))
            throw new IllegalArgumentException("errore");
        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("Teatro", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.INTRATTENIMENTO, curatore)));

        assertThrows(UnsupportedOperationException.class, () -> {
            assert puntoInteresse != null;
            curatoreServiceImpl.condividi(curatore, puntoInteresse);
        });
        //TODO test condivisione contenuto
    }

    @Test
    public void metodoCercaTurista() {

        Comune comune = comuneService.creaComune(new Comune(new RichiestaCreazioneComuneDTO("Milano")));
        TuristaAutenticato turista1 = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(comune,new TuristaAutenticatoDTO( "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "7Unico@", "user7"),Ruolo.CONTRIBUTOR));
        if (!(turista1 instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");


        TuristaAutenticato turista2 = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(comune,new TuristaAutenticatoDTO( "mirco", "blu", new GregorianCalendar(2002, GregorianCalendar.MAY, 15), "8Unico@", "user8"),Ruolo.CURATORE));
        if (!(turista2 instanceof Curatore curatore))
            throw new IllegalArgumentException("errore");

        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("Edicola", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.ATTIVITA_COMMERCIALE, contributor)));


        int numeroTagEdicolaIniziale = turistaService.findByTag((new RichiestaCreazioneTagDTO("Edicola", puntoInteresse))).size();


        poiService.aggiungiTag(puntoInteresse, new Tag(new RichiestaCreazioneTagDTO("Edicola", puntoInteresse)));
        assertEquals(numeroTagEdicolaIniziale + 1, turistaService.findByTag((new RichiestaCreazioneTagDTO("Edicola", puntoInteresse))).size());

        assert puntoInteresse != null;
        curatoreServiceImpl.valuta(curatore, puntoInteresse, Stato.APPROVATO);


        poiService.aggiungiTag(puntoInteresse, new Tag(new RichiestaCreazioneTagDTO("Tabaccheria", puntoInteresse)));

        assertEquals(numeroTagEdicolaIniziale + 1, turistaService.findByTag((new RichiestaCreazioneTagDTO("Edicola", puntoInteresse))).size());

        assertEquals("Edicola", poiService.getTags(puntoInteresse).getFirst().getValore());
        assertEquals("Tabaccheria", poiService.getTags(puntoInteresse).getLast().getValore());


        poiService.aggiungiTag(puntoInteresse, new Tag(new RichiestaCreazioneTagDTO("Bar", puntoInteresse)));

        assertEquals(numeroTagEdicolaIniziale + 1, turistaService.findByTag((new RichiestaCreazioneTagDTO("Edicola", puntoInteresse))).size());

        assertEquals("Edicola", poiService.getTags(puntoInteresse).getFirst().getValore());
        assertEquals("Tabaccheria", poiService.getTags(puntoInteresse).get(1).getValore());
        assertEquals("Bar", poiService.getTags(puntoInteresse).getLast().getValore());

    }

    @Test
    public void aggiungiFoto() {
        Comune comune = comuneService.creaComune(new Comune(new RichiestaCreazioneComuneDTO("Milano")));

        TuristaAutenticato turista = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(comune,new TuristaAutenticatoDTO( "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "9Unico@", "user9"),Ruolo.CONTRIBUTOR));
        if (!(turista instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");

        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("parco centrale", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.PARCO, contributor)));

        TuristaAutenticato turist2 = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(comune,new TuristaAutenticatoDTO( "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "9Unico@", "user99"),Ruolo.CURATORE));
        if (!(turist2 instanceof Curatore curatore1))
            throw new IllegalArgumentException("errore");

        assert puntoInteresse != null;
        curatoreServiceImpl.valuta(curatore1, puntoInteresse, Stato.APPROVATO);
        assertEquals(Boolean.TRUE, puntoInteresse.getStato().asBoolean());
        TuristaAutenticato turistaAutenticato = gestorePiattaformaService.registraTurista(new TuristaAutenticatoDTO( "andrea", "neri", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "10Unico@", "user10"));


        assertEquals(0, poiService.getMaterialiPoi(puntoInteresse).size());
        MaterialeGenerico foto = new Foto(turistaAutenticato);
        poiService.aggiungiMateriale(turistaAutenticato, puntoInteresse, foto);
        assertEquals(1, poiService.getMaterialiPoi(puntoInteresse).size());
        assertNull(foto.getStato().asBoolean());
        curatoreServiceImpl.valuta(curatore1, foto, Stato.APPROVATO);
        assertTrue(foto.getStato().asBoolean());
    }

    @Test
    public void segnalaContenuto() {
        Comune comune = comuneService.creaComune(new Comune(new RichiestaCreazioneComuneDTO("Milano")));

        TuristaAutenticato turista = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(comune,new TuristaAutenticatoDTO( "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "9Unico@", "user91"),Ruolo.CONTRIBUTOR));
        if (!(turista instanceof Contributor contributor))
            throw new IllegalArgumentException("errore");

        PuntoInteresse puntoInteresse = poiService.creaPuntoInteresse(new PuntoInteresse(new PuntoInteresseDTO("n0me b4rutt0 ", new Punto(comune.getPosizione().getLatitudine() + 0.015, comune.getPosizione().getLongitudine() + 0.015), new Orario(), TipologiaPuntoInteresse.PARCO, contributor)));


        TuristaAutenticato turista2 = gestorePiattaformaService.registraContributor(new RichiestaCreazioneContributorDTO(comune,new TuristaAutenticatoDTO( "mario", "rossi", new GregorianCalendar(2000, GregorianCalendar.MARCH, 17), "9Unico@", "user92"),Ruolo.CURATORE));
        if (!(turista2 instanceof Curatore curatore))
            throw new ClassCastException("Non è possibile trasformare il turista " + turista2 + " in un Curatore");
        assertEquals(0, turistaAutenticatoService.visualizzaNotifiche(curatore.getUsername()).size());
        assert puntoInteresse != null;
        turistaService.report(new PuntoInteresseDTO(puntoInteresse.getNome(),puntoInteresse.getPt(),puntoInteresse.getOrario(), puntoInteresse.getTipo(), puntoInteresse.getCreatore()), "Nome Sbagliato");

        assertEquals(1, turistaAutenticatoService.visualizzaNotifiche(curatore.getUsername()).size());


    }


}