package ids.unicam.DataBase;

import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Objects;

import static ids.unicam.Main.logger;

@Component
public class ConnessioneDatabase {

    private final Environment env;
    private static @Nullable Connection connection = null;

    @Autowired
    public ConnessioneDatabase(Environment env) {
        this.env = env;
    }

    public @Nullable Connection connessioneAlDatabase() {
        try {
            connection = DriverManager.getConnection(
                    Objects.requireNonNull(env.getProperty("spring.datasource.url")),
                    env.getProperty("spring.datasource.username"),
                    env.getProperty("spring.datasource.password"));
            logger.debug("Connessione al database stabilita");
            return connection;
        } catch (SQLException e) {
            logger.error("Connessione al database fallita", e);
            return null; //TODO magari va gestito?
        }
    }
}

