package ids.unicam;

import ids.unicam.models.contenuti.POIFactory.AttivitaFactory;
import ids.unicam.models.contenuti.POIFactory.Museo;
import ids.unicam.models.contenuti.POIFactory.MuseoFactory;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;

import java.time.LocalDate;
import java.util.Optional;

// Press Shift twice to open the Search Everywhere dialog and type `show whitespaces`,
// then press Enter. You can now see whitespace characters in your code.
public class Main {
    public static void main(String[] args) {

        AttivitaFactory attivitaFactory=new AttivitaFactory(LocalDate.now());
        PuntoInteresse punto= attivitaFactory.creaPoi("attività",new Punto(1,1),LocalDate.now());

        MuseoFactory museoFactory=new MuseoFactory();
        PuntoInteresse p2 = museoFactory.creaPoi("museo",new Punto(1,1));


    }
}