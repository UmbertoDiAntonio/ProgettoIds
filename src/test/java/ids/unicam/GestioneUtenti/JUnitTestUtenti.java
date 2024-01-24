package ids.unicam.GestioneUtenti;

import ids.unicam.controller.ContenutoController;
import ids.unicam.controller.ContestController;
import ids.unicam.controller.UtentiController;
import ids.unicam.models.Comune;
import ids.unicam.models.Gradi;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.attori.GestorePiattaforma;
import ids.unicam.models.contenuti.POIFactory.MuseoFactory;
import ids.unicam.utilites.Punto;
import org.junit.Test;

import java.util.Date;

import static org.junit.Assert.assertEquals;

public class JUnitTestUtenti {
    @Test
    public void generazioneUtenti() {

        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("nome", gestorePiattaforma, new ContenutoController(), new ContestController(), new UtentiController());

        gestorePiattaforma.getGestoreController().registraTurista("Mario", "Rossi", new Date(), "pass", "user");
        gestorePiattaforma.getGestoreController().registraTurista("Paolo", "Giallo", new Date(), "pass", "user");

        gestorePiattaforma.getGestoreController().registraContributor(comune, "Peppe", "Peppe", new Date(), "PASS", "user");
        assertEquals(1, comune.getContributors().size());

        gestorePiattaforma.promuovi(comune, comune.getContributors().get(0), Gradi.Curatore);

        assertEquals(0, comune.getContributors().size());
        assertEquals(1, comune.getCuratori().size());

        gestorePiattaforma.promuovi(comune, comune.getCuratori().get(0), Gradi.Contributor);

        assertEquals(1, comune.getContributors().size());
        assertEquals(0, comune.getCuratori().size());

        gestorePiattaforma.promuovi(comune, comune.getContributors().get(0), Gradi.ContributorTrusted);

        assertEquals(1, comune.getContributorTrusteds().size());
        assertEquals(0, comune.getCuratori().size());

    }

    @Test
    public void approvaContenutoCuratore(){
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        Comune comune = new Comune("nome", gestorePiattaforma, new ContenutoController(), new ContestController(), new UtentiController());

        gestorePiattaforma.getGestoreController().registraContributor(comune, "Peppe", "Peppe", new Date(), "PASS", "user");
        gestorePiattaforma.promuovi(comune, comune.getContributors().get(0), Gradi.Curatore);
        Curatore curatore = comune.getCuratori().getFirst();

        gestorePiattaforma.getGestoreController().registraContributor(comune, "Luca", "Rossi", new Date(), "pass1", "user2");
        Contributor contributor = comune.getContributors().get(0);
        contributor.addPuntoInteresse(new MuseoFactory().creaPoi("Accademia", new Punto(comune.getPosizione().getLatitudine()+0.1,comune.getPosizione().getLongitudine()+0.1)));
        assertEquals(false, comune.getContenutoController().getContenuti().getFirst().isApproved());
        curatore.aggiungiOsservatore(contributor);
        curatore.approva(comune.getContenutoController().getContenuti().getFirst(),true);
        assertEquals(true, comune.getContenutoController().getContenuti().getFirst().isApproved());

        comune.getContenutoController().getContenuti().getFirst().creaMateriale(false, contributor);
        assertEquals(false, comune.getContenutoController().getContenuti().getFirst().getMaterialeList().getFirst().isApproved());
        curatore.approva(comune.getContenutoController().getContenuti().getFirst().getMaterialeList().getFirst(), true);
        assertEquals(true, comune.getContenutoController().getContenuti().getFirst().getMaterialeList().getFirst().isApproved());

    }
}
