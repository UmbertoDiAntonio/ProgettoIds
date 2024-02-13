package ids.unicam;


import ids.unicam.DataBase.GestoreDatabase;
import ids.unicam.models.Service.GestorePiattaformaService;
import ids.unicam.models.attori.TuristaAutenticato;
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
@ComponentScan(basePackages = {"ids.unicam.DataBase", "ids.unicam.models.attori", "ids.unicam.models.Service", "ids.unicam.models.Repository","ids.unicam.controller"})
public class Main implements ApplicationRunner {
    public static final Logger logger = LoggerFactory.getLogger(Main.class);



    private final GestoreDatabase gestoreDatabase;
    private final GestorePiattaformaService gestorePiattaformaService;
    public Main(GestoreDatabase gestoreDatabase, GestorePiattaformaService gestorePiattaformaService) {
        this.gestoreDatabase = gestoreDatabase;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }
    public static void main(String[] args) {
        SpringApplication.run(Main.class, args);
    }

    @Override
    public void run(ApplicationArguments args){
        logger.info("this is a info message");
        logger.warn("this is a warn message");
        logger.error("this is a error message");
        gestoreDatabase.inizializzaDatabase();
        gestorePiattaformaService.registraTurista("Leonardo", "Compagnucci", new GregorianCalendar(1998, Calendar.JANUARY,1), "UNICAM", "leocompa");
        gestorePiattaformaService.registraTurista("Andrea", "Compagnucci", new GregorianCalendar(1998, Calendar.JANUARY,1), "UNICAM", "leocompa");
        gestorePiattaformaService.registraTurista("Umberto", "Di Antonio", new GregorianCalendar(1999,Calendar.NOVEMBER,23), "ciao!", "umber");
        logger.error("this is a error message");
        gestorePiattaformaService.eliminaTurista(gestorePiattaformaService.getTuristi().getLast());
        TuristaAutenticato turistaAutenticato1 = gestorePiattaformaService.cercaTurista(gestorePiattaformaService.getTuristi().getLast().getId());
        if(turistaAutenticato1 == null){
            System.out.println("ERRORE");
        }else
            System.out.println(turistaAutenticato1.getNome());
        for(TuristaAutenticato turistaAutenticato : gestorePiattaformaService.getTuristi())
            System.out.println(turistaAutenticato.getId());
        gestorePiattaformaService.eliminaListaTuristi();

    }

    @RequestMapping(value="/")
    public String hello(){
        return "Hello World";
    }

}