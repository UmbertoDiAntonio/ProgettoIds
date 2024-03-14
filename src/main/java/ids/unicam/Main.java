package ids.unicam;


import ids.unicam.DataBase.GestoreDatabase;
import ids.unicam.Service.GestorePiattaformaService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.core.env.Environment;

import java.io.IOException;


@SpringBootApplication
@ComponentScan(basePackages = {"ids.unicam.DataBase", "ids.unicam.models.attori", "ids.unicam.Service", "ids.unicam.DataBase.Repository", "ids.unicam.controller"})
public class Main implements ApplicationRunner {
    public static final Logger logger = LoggerFactory.getLogger(Main.class);

    private static Environment environment;
    public static boolean debug = false;

    private final GestoreDatabase gestoreDatabase;
    private final GestorePiattaformaService gestorePiattaformaService;

    @Autowired
    public Main(Environment environment, GestoreDatabase gestoreDatabase, GestorePiattaformaService gestorePiattaformaService) {
        Main.environment = environment;
        this.gestoreDatabase = gestoreDatabase;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }

    public static void main(String[] args) {
        SpringApplication.run(Main.class, args);

        String port = environment.getProperty("server.port");
        String urlToOpen = "http://localhost:" + port + "/swagger-ui/index.html#/";
        String urlToDB = "http://localhost:" + port + "/h2-console";

        openUrl(urlToOpen);
        openUrl(urlToDB);

        debug = Boolean.parseBoolean(environment.getProperty("debug"));

    }

    private static void openUrl(String url) {
        String os = System.getProperty("os.name").toLowerCase();
        Runtime rt = Runtime.getRuntime();

        String[] command = new String[1];
        try {
            if (os.contains("win")) {
                // Windows
                command = ("rundll32 url.dll,FileProtocolHandler " + url).split(" ");
            } else if (os.contains("mac")) {
                // Mac
                command = ("open " + url).split(" ");
            } else if (os.contains("nix") || os.contains("nux")) {
                // Linux o Unix
                command = ("xdg-open " + url).split(" ");
            }

            rt.exec(command);

        } catch (IOException e) {
            logger.error("Impossibile Eseguire il comando di apertura delle pagine web", e);
        }
    }


    @Override
    public void run(ApplicationArguments args) {
        gestoreDatabase.inizializzaDatabase();

        gestorePiattaformaService.creaGestore("admin", "admin");
    }


}
