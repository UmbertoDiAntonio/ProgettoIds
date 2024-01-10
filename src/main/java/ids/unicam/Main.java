package ids.unicam;

import ids.unicam.controller.ContenutoController;
import ids.unicam.controller.ContestController;
import ids.unicam.models.Comune;
import ids.unicam.models.Tempo;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;

// Press Shift twice to open the Search Everywhere dialog and type `show whitespaces`,
// then press Enter. You can now see whitespace characters in your code.
public class Main {
    public static void main(String[] args) {

        ContenutoController contenutoController=ContenutoController.getInstance();
        System.out.println(contenutoController.getContenuti().size());

        GestorePiattaforma gestore = new GestorePiattaforma();
        Comune comune=new Comune("jesi",
                new Punto(0,0),
                gestore);

        System.out.println("pos comune"+comune.getPosizione());
        Turista turista = new Turista();
        turista.search();

        TuristaLoggato turistaLoggato = new TuristaLoggato();
        System.out.println("id "+turistaLoggato.getId());
        TuristaLoggato turistaLoggato2 = new TuristaLoggato();
        System.out.println("id "+turistaLoggato2.getId());
        Contributor contributor = new Contributor(comune);

        PuntoInteresse puntoInteresse = new PuntoInteresse("n",new Punto(1,1));

        System.out.println("A "+contenutoController.getContenuti().size());
        System.out.println("punto "+puntoInteresse.getNome()+" "+puntoInteresse.getPt()+" "+puntoInteresse.isApproved());

        contributor.addPuntoInteresse(puntoInteresse);
        contributor.aggiungiScadenzaContenuto(puntoInteresse,new Tempo());
        System.out.println("B "+contenutoController.getContenuti().size());

        Curatore curatore = new Curatore(comune);
        curatore.approva(puntoInteresse);
        System.out.println("punto "+puntoInteresse.getNome()+" "+puntoInteresse.getPt()+" "+puntoInteresse.isApproved());

        ContributorTrusted contributorTrusted = new ContributorTrusted(comune);
        PuntoInteresse puntoInteresse2 = new PuntoInteresse("b", new Punto(10, 10));
        contributorTrusted.addPuntoInteresse(puntoInteresse2);
        System.out.println("C "+contenutoController.getContenuti().size());

        System.out.println("punto "+puntoInteresse2.getNome()+" "+puntoInteresse2.getPt()+" "+puntoInteresse2.isApproved());

        Animatore animatore = new Animatore(comune,new ContestController());
        System.out.println("D "+contenutoController.getContenuti().size());
        animatore.creaContest(true,"foto migliore del mese");
        System.out.println("E "+contenutoController.getContenuti().size());



    }
}