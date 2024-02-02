package ids.unicam.DataBase;

import ids.unicam.models.attori.GestorePiattaforma;
import ids.unicam.models.attori.TuristaAutenticato;
import jakarta.annotation.PostConstruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

import java.sql.*;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Objects;

@Component
public class DatabaseManager {

    @Autowired
    private Environment env;

    @PostConstruct
    public void creaDB() {
        try {
            System.out.println(env.getProperty("spring.datasource.url"));
            System.out.println(env.getProperty("spring.datasource.username"));
            System.out.println(env.getProperty("spring.datasource.password"));
            GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
            gestorePiattaforma.getGestoreController().registraTurista("Leonardo", "Compagnucci", new GregorianCalendar(1998, Calendar.JANUARY,1), "UNICAM", "leocompa");
            gestorePiattaforma.getGestoreController().registraTurista("Umberto", "Di Antonio", new GregorianCalendar(1999,Calendar.NOVEMBER,23), "ciao!", "umber");

            Class.forName("org.h2.Driver");

            try (Connection connection = DriverManager.getConnection(Objects.requireNonNull(env.getProperty("spring.datasource.url")),
                    env.getProperty("spring.datasource.username"),
                    env.getProperty("spring.datasource.password"))) {
                String createTableSQL =
                        "CREATE TABLE IF NOT EXISTS TURISTI(" +
                                "id INT PRIMARY KEY AUTO_INCREMENT," +
                                "name VARCHAR(50) NOT NULL," +
                                "surname VARCHAR(50) NOT NULL," +
                                "username VARCHAR(50) NOT NULL," +
                                "password VARCHAR(50) NOT NULL)";

                try (Statement statement = connection.createStatement()) {
                    statement.executeUpdate(createTableSQL);

                    List<TuristaAutenticato> turisti = gestorePiattaforma.getGestoreController().getUtentiController().getTuristi();

                    for (TuristaAutenticato turistaAutenticato : turisti) {
                        String insertDataSQL = "INSERT INTO TURISTI (name, surname, username, password) VALUES ('" +
                                turistaAutenticato.getNome() + "', '" +
                                turistaAutenticato.getCognome() + "', '" +
                                turistaAutenticato.getUsername() + "', '" +
                                turistaAutenticato.getPassword() + "')";
                        statement.addBatch(insertDataSQL);
                    }

                    statement.executeBatch();

                    String selectSQL = "SELECT * FROM TURISTI WHERE username = 'leocompa'";
                    try (ResultSet resultSet = statement.executeQuery(selectSQL)) {
                        while (resultSet.next()) {
                            int id = resultSet.getInt("id");
                            String username = resultSet.getString("username");
                            String password = resultSet.getString("password");
                            System.out.println("ID: " + id + ", Username: " + username + ", Password: " + password);
                        }
                    }
                }
                connection.commit();
            }

            System.out.println("Operazioni sul database H2 completate con successo.");
        } catch (ClassNotFoundException | SQLException e) {
            e.printStackTrace();
        }
    }
}
