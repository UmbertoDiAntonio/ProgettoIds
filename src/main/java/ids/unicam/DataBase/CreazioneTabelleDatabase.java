package ids.unicam.DataBase;

import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Component;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;

import static ids.unicam.Main.logger;

@Component
public class CreazioneTabelleDatabase {
    private final int NUMERO_MASSIMO_TAPPE = 20;

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
            logger.error("Impossibile eseguire la QuerySQL creazione tabelel Turista", e);
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
            logger.error("Impossibile eseguire la QuerySQL creazione tabelel Contributor", e);
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
            logger.error("Impossibile eseguire la QuerySQL creazione tabelel ContributorAutorizzati", e);
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
            logger.error("Impossibile eseguire la QuerySQL creazione tabelel Animatori", e);
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
            logger.error("Impossibile eseguire la QuerySQL creazione tabelel Curatori", e);
        }
    }

    private void creaTabellaPOI(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS PUNTI_DI_INTERESSE(" +
                        "id INT PRIMARY KEY NOT NULL," +
                        "stato BOOLEAN NOT NULL," +
                        "latitudine DOUBLE NOT NULL," +
                        "longitudine DOUBLE NOT NULL," +
                        "tag VARCHAR(50)," +
                        "nome VARCHAR(50) NOT NULL," +
                        "orario VARCHAR(50)," +
                        "tipo VARCHAR(50) NOT NULL)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();

        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabelel POI", e);
        }
    }

    private void creaTabellaItinerari(@NotNull Connection connection) {
        StringBuilder createTableSQL =
                new StringBuilder("CREATE TABLE IF NOT EXISTS ITINERARI(" +
                        "id INT PRIMARY KEY AUTO_INCREMENT," +
                        "tag VARCHAR(50)," +
                        "nome VARCHAR(50) NOT NULL,");
        for (int i = 1; i < NUMERO_MASSIMO_TAPPE; i++) {
            createTableSQL.append("tappa_").append(i).append(" INT NOT NULL");
            if (i < NUMERO_MASSIMO_TAPPE - 1)
                createTableSQL.append(",");
        }
        createTableSQL.append(")");
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL.toString())) {
            statement.executeUpdate();

        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabelel Itinerari", e);
        }
    }

    private void creaTabellaContest(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS CONTEST(" +
                        "id INT PRIMARY KEY NOT NULL," +
                        "tag VARCHAR(50)," +
                        "nome VARCHAR(50) NOT NULL," +
                        "aperto BOOLEAN NOT NULL," +
                        "obiettivo VARCHAR(100) NOT NULL," +
                        "id_creatore INT NOT NULL," +
                        "partecipanti VARCHAR(500) )";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabelel Contest", e);
        }
    }

    private void creaTabellaMateriali(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS MATERIALI(" +
                        "id INT PRIMARY KEY NOT NULL," +
                        "stato BOOLEAN NOT NULL," +
                        "id_creatore INT NOT NULL," +
                        "tipo VARCHAR(50) NOT NULL)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabelel Materiali", e);
        }
    }

    private void creaTabellaComuni(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS COMUNI(" +
                        "comune VARCHAR(50) PRIMARY KEY NOT NULL," +
                        "latitudine DOUBLE NOT NULL," +
                        "longitudine DOUBLE NOT NULL)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabelel Comuni", e);
        }
    }
}
