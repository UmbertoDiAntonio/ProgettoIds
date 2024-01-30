package ids.unicam.GestioneUtenti;

import ids.unicam.controller.ContenutoController;
import ids.unicam.controller.ContestController;
import ids.unicam.Comune;
import ids.unicam.models.Ruolo;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.attori.GestorePiattaforma;
import ids.unicam.models.contenuti.Foto;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.POIFactory.MuseoFactory;
import ids.unicam.utilites.Punto;
import org.junit.jupiter.api.Test;


import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;


public class JUnitUtentiTest {
    @Test
    public void generazioneUtenti() {

        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("nome", gestorePiattaforma, new ContenutoController(), new ContestController());

        gestorePiattaforma.getGestoreController().registraTurista("Mario", "Rossi", new Date(), "pass", "user");
        gestorePiattaforma.getGestoreController().registraTurista("Paolo", "Giallo", new Date(), "pass", "user");

        gestorePiattaforma.getGestoreController().registraContributor(comune, "Peppe", "Peppe", new Date(), "PASS", "user");
        assertEquals(1, comune.getContributors().size());

        gestorePiattaforma.promuovi( comune.getContributors().getFirst(), Ruolo.Curatore);

        assertEquals(0, comune.getContributors().size());
        assertEquals(1, comune.getCuratori().size());

        gestorePiattaforma.promuovi( comune.getCuratori().getFirst(), Ruolo.Contributor);

        assertEquals(1, comune.getContributors().size());
        assertEquals(0, comune.getCuratori().size());

        gestorePiattaforma.promuovi(comune.getContributors().getFirst(), Ruolo.ContributorTrusted);

        assertEquals(1, comune.getContributorTrusteds().size());
        assertEquals(0, comune.getCuratori().size());

    }

    @Test
    public void approvaContenutoCuratore(){
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("nome", gestorePiattaforma, new ContenutoController(), new ContestController());

        gestorePiattaforma.getGestoreController().registraContributor(comune, "Peppe", "Peppe", new Date(), "PASS", "user");
        gestorePiattaforma.promuovi( comune.getContributors().getFirst(), Ruolo.Curatore);
        Curatore curatore = comune.getCuratori().getFirst();

        gestorePiattaforma.getGestoreController().registraContributor(comune, "Luca", "Rossi", new Date(), "pass1", "user2");
        Contributor contributor = comune.getContributors().getFirst();
        contributor.addPuntoInteresse(new MuseoFactory().creaPoi("Accademia", new Punto(comune.getPosizione().getLatitudine()+0.01,comune.getPosizione().getLongitudine()+0.01)));
        assertFalse(comune.getContenutoController().getContenuti().getFirst().isApproved());
        curatore.aggiungiOsservatore(contributor);
        curatore.valuta(comune.getContenutoController().getContenuti().getFirst(),true);
        assertTrue(comune.getContenutoController().getContenuti().getFirst().isApproved());
        Materiale materiale1 = new Foto( contributor);
        comune.getContenutoController().getContenuti().getFirst().addMateriale(materiale1);
        assertFalse(comune.getContenutoController().getContenuti().getFirst().getMaterialeList().getFirst().isApproved());
        curatore.valuta(comune.getContenutoController().getContenuti().getFirst().getMaterialeList().getFirst(), true);
        assertTrue(comune.getContenutoController().getContenuti().getFirst().getMaterialeList().getFirst().isApproved());

    }
}
