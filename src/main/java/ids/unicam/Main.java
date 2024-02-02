package ids.unicam;



import ids.unicam.models.attori.GestorePiattaforma;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ids.unicam.DataBase.DatabaseManager;

import java.util.Calendar;
import java.util.GregorianCalendar;

@RestController
@SpringBootApplication
public class Main implements ApplicationRunner {

    public static void main(String[] args) {
        SpringApplication.run(Main.class, args);
    }

    @Override
    public void run(ApplicationArguments args){
        GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
        gestorePiattaforma.getGestoreController().registraTurista("Leonardo", "Compagnucci", new GregorianCalendar(1998, Calendar.JANUARY,1), "UNICAM", "leocompa");
        gestorePiattaforma.getGestoreController().registraTurista("Umberto", "Di Antonio", new GregorianCalendar(1999,Calendar.NOVEMBER,23), "ciao!", "umber");
        System.out.println("Esecuzione Corretta!");
    }

    @RequestMapping(value="/")
    public String hello(){
        return "Hello World";
    }


}