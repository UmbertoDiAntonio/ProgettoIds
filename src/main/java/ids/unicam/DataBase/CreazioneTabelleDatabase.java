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
        creaTabellaInvito(connection);
        creaTabellaTag(connection);
        creaTabellaTagPuntoInteresse(connection);
        creaTabellaTagItinerario(connection);
        creaTabellaPreferiti(connection);
        creaTabellaTagContest(connection);
        creaTabellaOsservatori(connection);
        creaTabellaListaMaterialiPuntoInteresse(connection);
        creaTabellaListaMaterialiContest(connection);
        creaTabellaNotifica(connection);
    }

    public void eliminaTabelle(@NotNull Connection connection) {
        String[] tableNames = {
                "NOTIFICA", "CONTEST_LISTA_MATERIALI", "PUNTI_DI_INTERESSE_LISTA_MATERIALI",
                "CURATORI_OSSERVATORI", "CONTEST_TAGS", "TURISTI_PREFERITI", "ITINERARIO_TAGS",
                "PUNTI_DI_INTERESSE_TAGS", "TAG", "INVITO", "PUNTO_INTERESSE_HOURS_MAP",
                "ITINERARI_PERCORSO", "CONTEST_PARTECIPANTI", "COMUNI", "MATERIALI",
                "CONTEST", "ITINERARI", "PUNTI_DI_INTERESSE",
                "CURATORI", "ANIMATORI", "CONTRIBUTOR_AUTORIZZATI",
                "CONTRIBUTOR", "TURISTI"
        };
        for (String table : tableNames) {

            String dropTableSQL = "DROP TABLE IF EXISTS " + table;
            try (PreparedStatement statement = connection.prepareStatement(dropTableSQL)) {
                statement.executeUpdate();
            } catch (SQLException e) {
                logger.error("Impossibile eseguire la QuerySQL elimina tabelle " + table, e);
            }
        }


    }
    private void creaTabellaListaMaterialiPuntoInteresse(@NotNull Connection connection){

        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS PUNTI_DI_INTERESSE_LISTA_MATERIALI(" +
                        "id INT AUTO_INCREMENT PRIMARY KEY," +
                        "punto_interesse_id INT," +
                        " lista_materiali_id INT," +
                        "FOREIGN KEY (punto_interesse_id) REFERENCES PUNTI_DI_INTERESSE(id) ON DELETE CASCADE)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Lista Materiali Punto Interesse", e);
        }
    }

    private void creaTabellaListaMaterialiContest(@NotNull Connection connection){

        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS CONTEST_LISTA_MATERIALI(" +
                        "contest_id INT," +
                        "lista_materiali_id INT," +
                        "PRIMARY KEY (contest_id, lista_materiali_id)," +
                        "FOREIGN KEY (contest_id) REFERENCES CONTEST(id) ON DELETE CASCADE,"+
                        "FOREIGN KEY (lista_materiali_id) REFERENCES MATERIALI(id) ON DELETE CASCADE)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Lista Materiali Punto Interesse", e);
        }
    }

    private void creaTabellaOsservatori(@NotNull Connection connection){

        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS CURATORI_OSSERVATORI(" +
                        "CURATORE_ID INT," +
                        "OSSERVATORI_ID INT," +
                        "PRIMARY KEY(CURATORE_ID, OSSERVATORI_ID)," +
                        "FOREIGN KEY (CURATORE_ID) REFERENCES CURATORI(id) ON DELETE CASCADE," +
                        "FOREIGN KEY (OSSERVATORI_ID) REFERENCES CONTRIBUTOR(id) ON DELETE CASCADE)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Osservatori", e);
        }
    }

    private void creaTabellaInvito(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS INVITO(" +
                        "id INT PRIMARY KEY AUTO_INCREMENT," +
                        "contest_id INT," +
                        "invitato_id INT," +
                        "FOREIGN KEY (contest_id) REFERENCES CONTEST(id))";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Invito", e);
        }
    }

    private void creaTabellaTag(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS TAG(" +
                        "id INT AUTO_INCREMENT," +
                        "VALORE VARCHAR(50)," +
                        "PUNTO_ID INT NOT NULL," +
                        "FOREIGN KEY (PUNTO_ID) REFERENCES PUNTI_DI_INTERESSE(ID) ON DELETE CASCADE," +
                        "PRIMARY KEY (id))";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Tag Punto Interesse", e);
        }
    }

    private void creaTabellaTagPuntoInteresse(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS PUNTI_DI_INTERESSE_TAGS(" +
                        "id INT AUTO_INCREMENT," +
                        "punto_interesse_id INT," +
                        "tags_id INT," +
                        "PRIMARY KEY (id,punto_interesse_id, tags_id)," +
                        "FOREIGN KEY (tags_id) REFERENCES TAG(id)," +
                        "FOREIGN KEY (punto_interesse_id) REFERENCES PUNTI_DI_INTERESSE(id))";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Tag Punto Interesse", e);
        }
    }

    private void creaTabellaTagItinerario(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS ITINERARIO_TAGS(" +
                        "itinerario_id INT," +
                        "tags VARCHAR(50)," +
                        "PRIMARY KEY (itinerario_id, tags)," +
                        "FOREIGN KEY (itinerario_id) REFERENCES ITINERARI(id))";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Tag Itinerario", e);
        }
    }

    private void creaTabellaPreferiti(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS TURISTI_PREFERITI(" +
                        "TURISTA_AUTENTICATO_ID INT," +
                        "PREFERITI_ID INT," +
                        "FOREIGN KEY (TURISTA_AUTENTICATO_ID) REFERENCES TURISTI(ID)," +
                        "FOREIGN KEY (PREFERITI_ID) REFERENCES PUNTI_DI_INTERESSE(ID) ON DELETE CASCADE)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Turisti Preferiti", e);
        }
    }

    private void creaTabellaTuristi(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS TURISTI(" +
                        "id INT PRIMARY KEY AUTO_INCREMENT," +
                        "nome VARCHAR(50) NOT NULL," +
                        "cognome VARCHAR(50) NOT NULL," +
                        "data_nascita DATE," +
                        "username VARCHAR(50) NOT NULL," +
                        "password VARCHAR(50) NOT NULL)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Turista", e);
        }
    }

    private void creaTabellaTagContest(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS CONTEST_TAGS(" +
                        "contest_id INT," +
                        "tags VARCHAR(50)," +
                        "tags_id INT," +
                        "PRIMARY KEY (contest_id, tags)," +
                        "FOREIGN KEY (contest_id) REFERENCES contest(id))";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Tag Contest", e);
        }
    }

    private void creaTabellaContributor(@NotNull Connection connection) {
        String createTableSQL = "CREATE TABLE IF NOT EXISTS CONTRIBUTOR(" +
                "id INT PRIMARY KEY NOT NULL," +
                "comune VARCHAR(50) NOT NULL," +
                "nome VARCHAR(50) NOT NULL," +
                "cognome VARCHAR(50) NOT NULL," +
                "data_nascita DATE," +
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
                        "data_nascita DATE," +
                        "username VARCHAR(50) NOT NULL," +
                        "password VARCHAR(50) NOT NULL)";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella ContributorAutorizzati", e);
        }
    }

    private void creaTabellaNotifica(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS NOTIFICA(" +
                        "id INT PRIMARY KEY NOT NULL," +
                        "data DATE," +
                        "descrizione VARCHAR(255),"+
                        "ricevente_id INT," +
                        "segnalatore_id INT,"+
                        "titolo VARCHAR(100),"+
                        "FOREIGN KEY (ricevente_id) REFERENCES CONTRIBUTOR(id),"+
                        "FOREIGN KEY (segnalatore_id) REFERENCES TURISTI(id))";
        try (PreparedStatement statement = connection.prepareStatement(createTableSQL)) {
            statement.executeUpdate();
        } catch (SQLException e) {
            logger.error("Impossibile eseguire la QuerySQL creazione tabella Notifica", e);
        }
    }

    private void creaTabellaAnimatori(@NotNull Connection connection) {
        String createTableSQL =
                "CREATE TABLE IF NOT EXISTS ANIMATORI(" +
                        "id INT PRIMARY KEY NOT NULL," +
                        "comune VARCHAR(50) NOT NULL," +
                        "nome VARCHAR(50) NOT NULL," +
                        "cognome VARCHAR(50) NOT NULL," +
                        "data_nascita DATE," +
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
                        "data_nascita DATE," +
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
                        "PRIMARY KEY (itinerario_id,percorso_id)," +
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
                        "nome VARCHAR(50) NOT NULL," +
                        "tipo VARCHAR(50) NOT NULL," +
                        "creatore_id INT NOT NULL,"+
                        "EXPIRE_DATE DATE)";
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
                        "nome VARCHAR(50) NOT NULL," +
                        "EXPIRE_DATE DATE," +
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
                        "nome_comune VARCHAR(50) NOT NULL," +
                        "nome VARCHAR(50) NOT NULL," +
                        "latitudine DOUBLE ," +
                        "longitudine DOUBLE ," +
                        "aperto BOOLEAN NOT NULL," +
                        "obiettivo VARCHAR(100) NOT NULL," +
                        "creatore_id INT NOT NULL," +
                        "Materiale_Vincitore_id INT,"+
                        "EXPIRE_DATE DATE)";
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
