package ids.unicam.DataBase;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.sql.Connection;
import java.sql.SQLException;

import static ids.unicam.Main.logger;

@Component
public class GestoreDatabase {
    private final ConnessioneDatabase connessioneDatabase;
    private final CreazioneTabelleDatabase creazioneTabelleDatabase;

    @Autowired
    public GestoreDatabase(ConnessioneDatabase connessioneDatabase, CreazioneTabelleDatabase creazioneTabelleDatabase) {
        this.connessioneDatabase = connessioneDatabase;
        this.creazioneTabelleDatabase = creazioneTabelleDatabase;
    }

    /**
     * Elimina le tabella del Database se esistono
     */
    public void eliminaTabelleDB() {
        try (Connection connection = getConnessioneDatabase().connessioneAlDatabase()) {
            if (connection == null) {
                logger.error("Impossibile connettersi al Database");
                return;
            }
            getCreazioneTabelleDatabase().eliminaTabelle(connection);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }


    /**
     * Stabilisce una connessione con il Database e ne genera le tabelle se non esistono
     */
    public void inizializzaDatabase() {
        try (Connection connection = getConnessioneDatabase().connessioneAlDatabase()) {
            if (connection == null) {
                logger.error("Impossibile connettersi al Database");
                return;
            }
            getCreazioneTabelleDatabase().inizializzaDatabase(connection);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    private ConnessioneDatabase getConnessioneDatabase() {
        return connessioneDatabase;
    }

    private CreazioneTabelleDatabase getCreazioneTabelleDatabase() {
        return creazioneTabelleDatabase;
    }

}
