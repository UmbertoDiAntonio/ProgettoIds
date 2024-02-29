package ids.unicam.DataBase;

import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Objects;

@Component
public class ConnessioneDatabase {

    private final Environment env;

    @Autowired
    public ConnessioneDatabase(Environment env) {
        this.env = env;
    }

    public @Nullable Connection connessioneAlDatabase() {
        Connection connection;
        try {
            connection = DriverManager.getConnection(
                    Objects.requireNonNull(env.getProperty("spring.datasource.url")),
                    env.getProperty("spring.datasource.username"),
                    env.getProperty("spring.datasource.password"));
        } catch (SQLException e) {
            throw new RuntimeException("Impossibile Stabilire una connessione al Database", e);
        }
        return connection;

    }
}

