package ids.unicam.DataBase;
import java.sql.*;

public class DatabaseManager {

        // URL di connessione al database H2 (memoria)
        private static final String DB_URL = "jdbc:h2:mem:testdb";

        // Nome utente e password del database (scegliere come preferisci)
        private static String user = "user";
        private static String password = "pass";

        public static void creaDB() {
            try {
                // Carica il driver JDBC per H2
                Class.forName("org.h2.Driver");

                // Crea la connessione al database
                Connection connection = DriverManager.getConnection(DB_URL, user, password);

                // Esegui le operazioni desiderate sul database
                // Esempio: crea una tabella
                String createTableSQL =
                        "CREATE TABLE CIAO (" +
                        "id INT PRIMARY KEY AUTO_INCREMENT," +
                        "username VARCHAR(50) NOT NULL," +
                        "password VARCHAR(50) NOT NULL)";
                connection.createStatement().executeUpdate(createTableSQL);

                // Esempio: inserisci dei dati nella tabella
                String insertDataSQL = "INSERT INTO CIAO (username, password) VALUES ('user1', 'password1')";
                //connection.createStatement().executeUpdate(insertDataSQL);

                Statement statement = connection.createStatement();
                int rowsAffected = statement.executeUpdate(insertDataSQL);
                System.out.println("Numero di righe inserite: " + rowsAffected);

                // Esegui una query di selezione per ottenere i dati appena inseriti
                String selectSQL = "SELECT * FROM CIAO WHERE username = 'user1'";
                ResultSet resultSet = statement.executeQuery(selectSQL);

                // Stampa i risultati della query
                while (resultSet.next()) {
                    int id = resultSet.getInt("id");
                    String username = resultSet.getString("username");
                    String password = resultSet.getString("password");
                    System.out.println("ID: " + id + ", Username: " + username + ", Password: " + password);
                }



                // Chiudi la connessione al database
                resultSet.close();
                statement.close();
                connection.commit();
                connection.close();

                System.out.println("Operazioni sul database H2 completate con successo.");
            } catch (ClassNotFoundException | SQLException e) {
                e.printStackTrace();
            }
        }
}
