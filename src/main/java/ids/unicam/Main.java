package ids.unicam;


import ids.unicam.DataBase.GestoreDatabase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.web.bind.annotation.RestController;


@RestController
@SpringBootApplication
@ComponentScan(basePackages = {"ids.unicam.DataBase", "ids.unicam.models.attori", "ids.unicam.Service", "ids.unicam.DataBase.Repository", "ids.unicam.controller"})
public class Main implements ApplicationRunner {
    public static final Logger logger = LoggerFactory.getLogger(Main.class);


    private final GestoreDatabase gestoreDatabase;

    public Main(GestoreDatabase gestoreDatabase) {
        this.gestoreDatabase = gestoreDatabase;
    }

    public static void main(String[] args) {
        SpringApplication.run(Main.class, args);
    }

    @Override
    public void run(ApplicationArguments args) {
        gestoreDatabase.inizializzaDatabase();
    }



}