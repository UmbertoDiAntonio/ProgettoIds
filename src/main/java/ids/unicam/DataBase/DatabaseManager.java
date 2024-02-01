package ids.unicam.DataBase;
import ids.unicam.controller.UtentiController;
import ids.unicam.models.attori.GestorePiattaforma;
import ids.unicam.models.attori.TuristaLoggato;

import java.sql.*;
import java.util.Date;
import java.util.List;

public class DatabaseManager {

    // URL di connessione al database H2 (memoria)
    private static final String DB_URL = "jdbc:h2:./src/main/resources/DBStorage/new_file";
    // Nome utente e password del database
    private static final String USER = "user";
    private static final String PASSWORD = "pass";

    public static void creaDB() {
        try {
            UtentiController utentiController = new UtentiController();
            GestorePiattaforma gestorePiattaforma = new GestorePiattaforma();
            gestorePiattaforma.getGestoreController().registraTurista("Leonardo","Compagnucci",new Date(), "UNICAM", "leocompa");
            // Carica il driver JDBC per H2
            Class.forName("org.h2.Driver");

            // Crea la connessione al database
            try (Connection connection = DriverManager.getConnection(DB_URL, USER, PASSWORD)) {
                // Esegui le operazioni desiderate sul database
                // Esempio: crea una tabella
                String createTableSQL =
                        "CREATE TABLE IF NOT EXISTS TURISTI(" +
                                "id VARCHAR(50) PRIMARY KEY," +
                                "name VARCHAR(50) NOT NULL," +
                                "surname VARCHAR(50) NOT NULL," +
                                "username VARCHAR(50) NOT NULL," +
                                "password VARCHAR(50) NOT NULL)";
                connection.createStatement().executeUpdate(createTableSQL);

                // Esempio: inserisci dei dati nella tabella
                String insertDataSQL = "INSERT INTO TURISTI (id, name, surname, username, password) VALUES (?, ?, ?, ?, ?)";
                Statement statement = connection.createStatement();
                List<TuristaLoggato> turisti = utentiController.getTuristi();

                for (TuristaLoggato turistaLoggato : turisti) {
                    String values = String.format("('%s', '%s', '%s', '%s', '%s')",
                            turistaLoggato.getId(), turistaLoggato.getName(), turistaLoggato.getSurname(),
                            turistaLoggato.getUsername(), turistaLoggato.getPassword());
                    statement.addBatch(insertDataSQL + values);
                }
statement.executeBatch();
                // Esegui una query di selezione per ottenere i dati appena inseriti
                String selectSQL = "SELECT * FROM TURISTI WHERE username = 'Leonardo'";
                ResultSet resultSet = statement.executeQuery(selectSQL);

                // Stampa i risultati della query
                while (resultSet.next()) {
                    String id = resultSet.getString("id");
                    String username = resultSet.getString("username");
                    String password = resultSet.getString("password");
                    System.out.println("ID: " + id + ", Username: " + username + ", Password: " + password);
                }
            }

            System.out.println("Operazioni sul database H2 completate con successo.");
        } catch (ClassNotFoundException | SQLException e) {
            e.printStackTrace();
        }
    }
}

