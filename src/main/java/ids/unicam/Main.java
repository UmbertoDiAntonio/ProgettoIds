package ids.unicam;


import ids.unicam.DataBase.GestoreDatabase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.core.env.Environment;
import org.springframework.web.bind.annotation.RestController;

import java.io.IOException;


@RestController
@SpringBootApplication
@ComponentScan(basePackages = {"ids.unicam.DataBase", "ids.unicam.models.attori", "ids.unicam.Service", "ids.unicam.DataBase.Repository", "ids.unicam.controller"})
public class Main implements ApplicationRunner {
    public static final Logger logger = LoggerFactory.getLogger(Main.class);

    private static Environment environment;

    private final GestoreDatabase gestoreDatabase;

    public Main(Environment environment, GestoreDatabase gestoreDatabase) {
        Main.environment = environment;
        this.gestoreDatabase = gestoreDatabase;
    }

    public static void main(String[] args) {
        SpringApplication.run(Main.class, args);
        // URL da aprire
        String port = environment.getProperty("server.port");
        String urlToOpen = "http://localhost:"+port+"/swagger-ui/index.html#/";
        String urlToDB = "http://localhost:"+port+"/h2-console";

        // Apri l'URL
        openUrl(urlToOpen);
        openUrl(urlToDB);
    }

    private static void openUrl(String url) {
        String os = System.getProperty("os.name").toLowerCase();
        Runtime rt = Runtime.getRuntime();

        String command ="";
        try {
            if (os.contains("win")) {
                // Windows
                command= "rundll32 url.dll,FileProtocolHandler " + url;
            } else if (os.contains("mac")) {
                // Mac
                command="open " + url;
            } else if (os.contains("nix") || os.contains("nux")) {
                // Linux o Unix
                command= "xdg-open " + url;
            }

            rt.exec(command);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    @Override
    public void run(ApplicationArguments args) {
        gestoreDatabase.inizializzaDatabase();
    }


}
