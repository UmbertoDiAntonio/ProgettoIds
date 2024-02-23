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

        String urlToOpen = "http://localhost:9090/swagger-ui/index.html#/";
        String urlToDB = "http://localhost:9090/h2-console";

        openUrl(urlToOpen);
        openUrl(urlToDB);
    }

    private static void openUrl(String url) {
        String os = System.getProperty("os.name").toLowerCase();
        Runtime rt = Runtime.getRuntime();

        try {
            if (os.contains("win")) {
                // Windows
                rt.exec("rundll32 url.dll,FileProtocolHandler " + url);
            } else if (os.contains("mac")) {
                // Mac
                rt.exec("open " + url);
            } else if (os.contains("nix") || os.contains("nux")) {
                // Linux o Unix
                rt.exec("xdg-open " + url);
            } else {
                // Sistema operativo non supportato
                System.out.println("Apertura URL non supportata su questo sistema operativo.");
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    @Override
    public void run(ApplicationArguments args) {
        gestoreDatabase.inizializzaDatabase();
    }


}