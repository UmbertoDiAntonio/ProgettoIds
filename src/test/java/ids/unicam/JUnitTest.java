package ids.unicam;

import ids.unicam.controller.ContenutoController;
import ids.unicam.controller.ContestController;
import ids.unicam.controller.UtentiController;
import ids.unicam.models.Comune;
import ids.unicam.models.Gradi;
import ids.unicam.models.attori.GestorePiattaforma;
import ids.unicam.models.attori.Turista;
import ids.unicam.utilites.Punto;
import org.junit.Test;

import java.util.Date;

import static org.junit.Assert.assertEquals;

public class JUnitTest {
    @Test
    public void testCoordinate() {
        {
            Punto first = new Punto(0, 0);
            Punto second = new Punto(10, 0);
            assertEquals(10, first.getDistance(second),0);
            assertEquals(100, first.getDistanceSquared(second),0);
        }
    }

    @Test
    public void creaTuristaLoggato() {
        {
            GestorePiattaforma gestorePiattaforma=new GestorePiattaforma();
            Comune comune=new Comune("nome",new Punto(1,1),gestorePiattaforma, new ContenutoController(), new ContestController(),new UtentiController());

            comune.getUtentiController().registraTurista(comune,"Mario","Rossi",new Date(),"pass","user");
            assertEquals(comune.getTuristi().size(),1);
            comune.getUtentiController().registraTurista(comune,"Paolo","Giallo",new Date(),"pass","user");
            assertEquals(comune.getTuristi().size(),2);

            comune.getUtentiController().registraContributor(comune,"Peppe","Peppe",new Date(),"PASS","user");
            assertEquals(1,comune.getContributors().size());

            gestorePiattaforma.promuovi(comune,comune.getContributors().get(0), Gradi.Curatore);

            assertEquals(0,comune.getContributors().size());
            assertEquals(1,comune.getCuratori().size());

            gestorePiattaforma.promuovi(comune,comune.getCuratori().get(0), Gradi.Contributor);

            assertEquals(1,comune.getContributors().size());
            assertEquals(0,comune.getCuratori().size());

            gestorePiattaforma.promuovi(comune,comune.getContributors().get(0), Gradi.ContributorTrusted);

            assertEquals(1,comune.getContributors().size());
            assertEquals(0,comune.getCuratori().size());
        }
    }
}


