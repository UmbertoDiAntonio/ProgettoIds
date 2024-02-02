package ids.unicam.DataBase;

import ids.unicam.models.attori.TuristaAutenticato;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

import java.sql.*;
import java.util.Objects;

@Component
public class DatabaseManager {
/*
    @Value("${spring.datasource.url}")
    private String dbUrl;

    @Value("${spring.datasource.username}")
    private String dbUsername;

    @Value("${spring.datasource.password}")
    private String dbPassword;

 */
@Autowired
private Environment env;


    public Connection connectToDatabase(){
        try {
            Connection connection = DriverManager.getConnection(
                    Objects.requireNonNull(env.getProperty("spring.datasource.url")),
                    env.getProperty("spring.datasource.username"),
                    env.getProperty("spring.datasource.password"));
            System.out.println("Connessione al database stabilita!");
            return connection;
        } catch (SQLException e) {
            e.printStackTrace();
            return null;
        }
    }

    public void createTuristiTable(Connection connection) throws SQLException {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS TURISTI(" +
                        "id INT PRIMARY KEY AUTO_INCREMENT," +
                        "name VARCHAR(50) NOT NULL," +
                        "surname VARCHAR(50) NOT NULL," +
                        "username VARCHAR(50) NOT NULL," +
                        "password VARCHAR(50) NOT NULL)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        }
    }

    public static void aggiungiTuristaAlDatabase(Connection connection, TuristaAutenticato turistaAutenticato) {
        try {
            String insertDataSQL = "INSERT INTO TURISTI (name, surname, username, password) VALUES ('" +
                    turistaAutenticato.getNome() + "', '" +
                    turistaAutenticato.getCognome() + "', '" +
                    turistaAutenticato.getUsername() + "', '" +
                    turistaAutenticato.getPassword() + "')";
            try (Statement statement = connection.createStatement()) {
                statement.executeUpdate(insertDataSQL);
            }
            System.out.println("Turista aggiunto al database!");
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }
}


