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
import java.io.IOException;


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
        // URL da aprire
        String urlToOpen = "http://localhost:9090/swagger-ui/index.html#/";
        String urlToDB = "http://localhost:9090/h2-console";

        // Apri l'URL
        openUrl(urlToOpen);
        openUrl(urlToDB);
    }

    // Metodo per aprire un URL
    private static void openUrl(String url) {
        try {
            Runtime.getRuntime().exec("open " + url);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    @Override
    public void run(ApplicationArguments args) {
        gestoreDatabase.inizializzaDatabase();
    }


}