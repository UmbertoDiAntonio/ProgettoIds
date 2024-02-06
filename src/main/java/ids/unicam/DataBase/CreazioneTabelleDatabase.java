package ids.unicam.DataBase;
import ids.unicam.Exception.ConnessioneFallitaException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.sql.*;

import static ids.unicam.Main.logger;
@Component
public class CreazioneTabelleDatabase {
    private final Connection connection;
    private final int numeroMassimoTappe = 20;


    @Autowired
    public CreazioneTabelleDatabase(ConnessioneDatabase connectionManager) {
        this.connection = connectionManager.connessioneAlDatabase();
    }

    public void inizializzaDatabase() throws SQLException, ConnessioneFallitaException {
        creaTabellaTuristi();
        creaTabellaContributor();
        creaTabellaContributorAutorizzati();
        creaTabellaAnimatori();
        creaTabellaCuratori();
        creaTabellaPOI();
        creaTabellaItinerari();
        creaTabellaContest();
        creaTabellaMateriali();
    }

    private void creaTabellaTuristi() throws SQLException, ConnessioneFallitaException {
        if (connection == null) {
            logger.error("errore creazione tabella turisti");
            throw new ConnessioneFallitaException("Stabilire una connesione al database");
        } else {
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
    }

    private void creaTabellaContributor() throws SQLException, ConnessioneFallitaException {
        if (connection == null) {
            logger.error("errore creazione tabella contributor");
            throw new ConnessioneFallitaException("Stabilire una connesione al database");
        } else {
            String createTableSQL =
                    "CREATE TABLE IF NOT EXISTS CONTRIBUTOR(" +
                            "id INT PRIMARY KEY AUTO_INCREMENT," +
                            "comune VARCHAR(50) NOT NULL," +
                            "name VARCHAR(50) NOT NULL," +
                            "surname VARCHAR(50) NOT NULL," +
                            "username VARCHAR(50) NOT NULL," +
                            "password VARCHAR(50) NOT NULL)";
            try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
                statement.executeUpdate();
            }
        }
    }

    private void creaTabellaContributorAutorizzati() throws SQLException, ConnessioneFallitaException {
        if (connection == null) {
            logger.error("errore creazione tabella contributor autorizzati");
            throw new ConnessioneFallitaException("Stabilire una connesione al database");
        } else {
            String createTableSQL =
                    "CREATE TABLE IF NOT EXISTS CONTRIBUTOR_AUTORIZZATI(" +
                            "id INT PRIMARY KEY AUTO_INCREMENT," +
                            "comune VARCHAR(50) NOT NULL," +
                            "name VARCHAR(50) NOT NULL," +
                            "surname VARCHAR(50) NOT NULL," +
                            "username VARCHAR(50) NOT NULL," +
                            "password VARCHAR(50) NOT NULL)";
            try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
                statement.executeUpdate();
            }
        }
    }

    private void creaTabellaAnimatori() throws SQLException, ConnessioneFallitaException {
        if (connection == null) {
            logger.error("errore creazione tabella animatori");
            throw new ConnessioneFallitaException("Stabilire una connesione al database");
        } else {
            String createTableSQL =
                    "CREATE TABLE IF NOT EXISTS ANIMATORI(" +
                            "id INT PRIMARY KEY AUTO_INCREMENT," +
                            "comune VARCHAR(50) NOT NULL," +
                            "name VARCHAR(50) NOT NULL," +
                            "surname VARCHAR(50) NOT NULL," +
                            "username VARCHAR(50) NOT NULL," +
                            "password VARCHAR(50) NOT NULL)";
            try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
                statement.executeUpdate();
            }
        }
    }

    private void creaTabellaCuratori() throws SQLException, ConnessioneFallitaException {
        if (connection == null) {
            logger.error("errore creazione tabella curatori");
            throw new ConnessioneFallitaException("Stabilire una connesione al database");
        } else {
            String createTableSQL =
                    "CREATE TABLE IF NOT EXISTS CURATORI(" +
                            "id INT PRIMARY KEY AUTO_INCREMENT," +
                            "comune VARCHAR(50) NOT NULL," +
                            "name VARCHAR(50) NOT NULL," +
                            "surname VARCHAR(50) NOT NULL," +
                            "username VARCHAR(50) NOT NULL," +
                            "password VARCHAR(50) NOT NULL)";
            try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
                statement.executeUpdate();
            }
        }
    }

    private void creaTabellaPOI() throws SQLException, ConnessioneFallitaException {
        if (connection == null) {
            logger.error("errore creazione tabella punti di interesse");
            throw new ConnessioneFallitaException("Stabilire una connesione al database");
        } else {
            String createTableSQL =
                    "CREATE TABLE IF NOT EXISTS PUNTI_DI_INTERESSE(" +
                            "id INT PRIMARY KEY AUTO_INCREMENT," +
                            "stato BOOLEAN NOT NULL," +
                            "latitudine DOUBLE NOT NULL," +
                            "longitudine DOUBLE NOT NULL," +
                            "tag VARCHAR(50)," +
                            "nome VARCHAR(50) NOT NULL," +
                            "orario VARCHAR(50)," +
                            "tipo VARCHAR(50) NOT NULL)";
            try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
                statement.executeUpdate();
            }
        }
    }

    private void creaTabellaItinerari() throws SQLException, ConnessioneFallitaException {
        if (connection == null) {
            logger.error("errore creazione tabella itinerari");
            throw new ConnessioneFallitaException("Stabilire una connesione al database");
        } else {
            StringBuilder createTableSQL =
                    new StringBuilder("CREATE TABLE IF NOT EXISTS ITINERARI(" +
                            "id INT PRIMARY KEY AUTO_INCREMENT," +
                            "tag VARCHAR(50)," +
                            "name VARCHAR(50) NOT NULL,");
            for (int i = 1; i < numeroMassimoTappe; i++) {
                createTableSQL.append("tappa_").append(i).append(" INT NOT NULL");
                if (i < numeroMassimoTappe - 1)
                    createTableSQL.append(",");
            }
            createTableSQL.append(")");
            try (PreparedStatement statement = connection.prepareStatement(createTableSQL.toString())) {
                statement.executeUpdate();
            }
        }
    }

    private void creaTabellaContest() throws SQLException, ConnessioneFallitaException {
        if (connection == null) {
            logger.error("errore creazione tabella contest");
            throw new ConnessioneFallitaException("Stabilire una connesione al database");
        } else {
            String createTableSQL =
                    "CREATE TABLE IF NOT EXISTS CONTEST(" +
                            "id INT PRIMARY KEY AUTO_INCREMENT," +
                            "tag VARCHAR(50)," +
                            "nome VARCHAR(50) NOT NULL," +
                            "aperto BOOLEAN NOT NULL," +
                            "obiettivo VARCHAR(100) NOT NULL," +
                            "id_creatore INT NOT NULL," +
                            "partecipanti VARCHAR(500) )";
            try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
                statement.executeUpdate();
            }
        }
    }

    private void creaTabellaMateriali() throws SQLException, ConnessioneFallitaException {
        if (connection == null){
            logger.error("errore creazione tabella materiali");
            throw new ConnessioneFallitaException("Stabilire una connesione al database");
        }else {
            String createTableSQL =
                    "CREATE TABLE IF NOT EXISTS MATERIALI(" +
                            "id INT PRIMARY KEY AUTO_INCREMENT," +
                            "stato BOOLEAN NOT NULL,"+
                            "id_creatore INT NOT NULL,"+
                            "tipo VARCHAR(50) NOT NULL)";
            try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
                statement.executeUpdate();
            }
        }
    }

}
