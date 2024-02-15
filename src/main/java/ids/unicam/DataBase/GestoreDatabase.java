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
    private final ModificaTabelleDatabase modificaTabelleDatabase;
    @Autowired
    public GestoreDatabase(ConnessioneDatabase connessioneDatabase, CreazioneTabelleDatabase creazioneTabelleDatabase, ModificaTabelleDatabase modificaTabelleDatabase) {
        this.connessioneDatabase = connessioneDatabase;
        this.creazioneTabelleDatabase = creazioneTabelleDatabase;
        this.modificaTabelleDatabase = modificaTabelleDatabase;
    }

    public void eliminaTabelleDB(){
        try (Connection connection = getConnessioneDatabase().connessioneAlDatabase()) {
            if(connection==null){
                logger.error("Impossibile connettersi al Database");
                return;
            }
            getCreazioneTabelleDatabase().eliminaTabelle(connection);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }


    public void inizializzaDatabase(){
        try (Connection connection = getConnessioneDatabase().connessioneAlDatabase()) {
            if(connection==null){
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

    private ModificaTabelleDatabase getModificaTabelleDatabase() {
        return modificaTabelleDatabase;
    }
}
