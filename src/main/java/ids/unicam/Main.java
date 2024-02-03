package ids.unicam;


import ids.unicam.models.attori.GestorePiattaforma;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Calendar;
import java.util.GregorianCalendar;

@RestController
@SpringBootApplication
@ComponentScan(basePackages = {"ids.unicam.DataBase", "ids.unicam.models.attori"})
public class Main implements ApplicationRunner {
    private static final Logger logger = LoggerFactory.getLogger(Main.class);



    public final GestorePiattaforma gestorePiattaforma;
    public Main(GestorePiattaforma gestorePiattaforma) {
        this.gestorePiattaforma = gestorePiattaforma;
    }
    public static void main(String[] args) {
        SpringApplication.run(Main.class, args);
    }

    @Override
    public void run(ApplicationArguments args){
        logger.info("this is a info message");
        logger.warn("this is a warn message");
        logger.error("this is a error message");
        gestorePiattaforma.getGestoreController().registraTurista("Leonardo", "Compagnucci", new GregorianCalendar(1998, Calendar.JANUARY,1), "UNICAM", "leocompa");
        gestorePiattaforma.getGestoreController().registraTurista("Umberto", "Di Antonio", new GregorianCalendar(1999,Calendar.NOVEMBER,23), "ciao!", "umber");
        logger.error("this is a error message");
        System.out.println("Esecuzione Corretta!");
    }

    @RequestMapping(value="/")
    public String hello(){
        return "Hello World";
    }


}