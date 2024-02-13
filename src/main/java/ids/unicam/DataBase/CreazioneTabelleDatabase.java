package ids.unicam.DataBase;

import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Component;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;

import static ids.unicam.Main.logger;

@Component
public class CreazioneTabelleDatabase {

    public void inizializzaDatabase(@NotNull Connection connection) {
        creaTabellaTuristi(connection);
        creaTabellaContributor(connection);
        creaTabellaContributorAutorizzati(connection);
        creaTabellaAnimatori(connection);
        creaTabellaCuratori(connection);
        creaTabellaPOI(connection);
        creaTabellaItinerari(connection);
        creaTabellaContest(connection);
        creaTabellaMateriali(connection);
        creaTabellaComuni(connection);
        creaTabellaContestPartecipanti(connection);
        creaTabellaItinerariPercorso(connection);
        creaTabellaOrariPuntoInteresse(connection);
    }

    private void creaTabellaTuristi(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS TURISTI(" +
                        "id INT PRIMARY KEY AUTO_INCREMENT," +
                        "nome VARCHAR(50) NOT NULL," +
                        "cognome VARCHAR(50) NOT NULL," +
                        "username VARCHAR(50) NOT NULL," +
                        "password VARCHAR(50) NOT NULL)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Turista", e);
        }
    }


    private void creaTabellaContributor(@NotNull Connection connection) {
        String createTableSQL = "CREATE TABLE IF NOT EXISTS CONTRIBUTOR(" +
                "id INT PRIMARY KEY NOT NULL," +
                "comune VARCHAR(50) NOT NULL," +
                "nome VARCHAR(50) NOT NULL," +
                "cognome VARCHAR(50) NOT NULL," +
                "username VARCHAR(50) NOT NULL," +
                "password VARCHAR(50) NOT NULL)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Contributor", e);
        }

    }

    private void creaTabellaContributorAutorizzati(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS CONTRIBUTOR_AUTORIZZATI(" +
                        "id INT PRIMARY KEY NOT NULL," +
                        "comune VARCHAR(50) NOT NULL," +
                        "nome VARCHAR(50) NOT NULL," +
                        "cognome VARCHAR(50) NOT NULL," +
                        "username VARCHAR(50) NOT NULL," +
                        "password VARCHAR(50) NOT NULL)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella ContributorAutorizzati", e);
        }
    }

    private void creaTabellaAnimatori(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS ANIMATORI(" +
                        "id INT PRIMARY KEY NOT NULL," +
                        "comune VARCHAR(50) NOT NULL," +
                        "nome VARCHAR(50) NOT NULL," +
                        "cognome VARCHAR(50) NOT NULL," +
                        "username VARCHAR(50) NOT NULL," +
                        "password VARCHAR(50) NOT NULL)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Animatori", e);
        }
    }

    private void creaTabellaCuratori(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS CURATORI(" +
                        "id INT PRIMARY KEY NOT NULL," +
                        "comune VARCHAR(50) NOT NULL," +
                        "nome VARCHAR(50) NOT NULL," +
                        "cognome VARCHAR(50) NOT NULL," +
                        "username VARCHAR(50) NOT NULL," +
                        "password VARCHAR(50) NOT NULL)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Curatori", e);
        }
    }

    private void creaTabellaContestPartecipanti(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS CONTEST_PARTECIPANTI(" +
                        "contest_id INT NOT NULL," +
                        "partecipanti_id INT," +
                        "FOREIGN KEY (contest_id) REFERENCES CONTEST(id)," +
                        "FOREIGN KEY (partecipanti_id) REFERENCES TURISTI(id))";

        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Contest Partecipanti", e);
        }
    }

    private void creaTabellaItinerariPercorso(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS ITINERARI_PERCORSO(" +
                        "itinerario_id INT," +
                        "percorso_id INT," +
                        "FOREIGN KEY (itinerario_id) REFERENCES ITINERARI(id)," +
                        "FOREIGN KEY (percorso_id) REFERENCES PUNTI_DI_INTERESSE(id))";

        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Itinerari Percorso", e);
        }
    }

    private void creaTabellaOrariPuntoInteresse(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS PUNTO_INTERESSE_HOURS_MAP(" +
                        "punto_interesse_id INT," +
                        "hours_map_key VARCHAR(50)," +
                        "closing_time TIME," +
                        "opening_time TIME," +
                        "PRIMARY KEY (punto_interesse_id, hours_map_key)," +
                        "FOREIGN KEY (punto_interesse_id) REFERENCES PUNTI_DI_INTERESSE(id))";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();

        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Orari Punto Interesse", e);
        }
    }

    private void creaTabellaPOI(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS PUNTI_DI_INTERESSE(" +
                        "id INT PRIMARY KEY NOT NULL," +
                        "nome_comune VARCHAR(50) NOT NULL," +
                        "stato BOOLEAN NOT NULL," +
                        "latitudine DOUBLE ," +
                        "longitudine DOUBLE ," +
                        "tag VARCHAR(50)," +
                        "nome VARCHAR(50) NOT NULL," +
                        "orario VARCHAR(50)," +
                        "tipo VARCHAR(50) NOT NULL)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();

        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella POI", e);
        }
    }

    private void creaTabellaItinerari(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS ITINERARI(" +
                        "nome_comune VARCHAR(50) NOT NULL," +
                        "nome VARCHAR(50) NOT NULL,"+
                        "id INT PRIMARY KEY AUTO_INCREMENT)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();

        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Itinerari", e);
        }
    }

    private void creaTabellaContest(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS CONTEST(" +
                        "id INT PRIMARY KEY NOT NULL," +
                //        "nome_comune VARCHAR(50) NOT NULL," +
                        "tag VARCHAR(50)," +
                        "nome VARCHAR(50) NOT NULL," +
                        "aperto BOOLEAN NOT NULL," +
                        "obiettivo VARCHAR(100) NOT NULL," +
                        "creatore_id INT NOT NULL," +
                        "partecipanti VARCHAR(500) )";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Contest", e);
        }
    }

    private void creaTabellaMateriali(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS MATERIALI(" +
                        "id INT PRIMARY KEY NOT NULL," +
                        "stato BOOLEAN NOT NULL," +
                        "creatore_id INT NOT NULL," +
                        "id_proprietario INT NOT NULL," +
                        "tipo VARCHAR(50) NOT NULL)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Materiali", e);
        }
    }

    private void creaTabellaComuni(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS COMUNI(" +
                        "comune VARCHAR(50) PRIMARY KEY NOT NULL," +
                        "latitudine DOUBLE," +
                        "longitudine DOUBLE )";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Comuni", e);
        }
    }
}
