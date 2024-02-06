package ids.unicam.DataBase;

import ids.unicam.models.attori.Turista;
import ids.unicam.models.attori.TuristaAutenticato;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.sql.*;

import static ids.unicam.Main.logger;

@Component
public class ModificaTabelleDatabase {

    private final Connection connection;

    @Autowired
    public ModificaTabelleDatabase(ConnessioneDatabase connectionManager) {
        this.connection = connectionManager.connessioneAlDatabase();
    }

    public void aggiungiTuristaAlDatabase(TuristaAutenticato turistaAutenticato) {
        try {
            String insertDataSQL = "INSERT INTO TURISTI (name, surname, username, password) VALUES ('" +
                    turistaAutenticato.getNome() + "', '" +
                    turistaAutenticato.getCognome() + "', '" +
                    turistaAutenticato.getUsername() + "', '" +
                    turistaAutenticato.getPassword() + "')";
            try (Statement statement = connection.createStatement()) {
                statement.executeUpdate(insertDataSQL);
            }
            logger.debug("Turista: " + turistaAutenticato.getNome() + " aggiunto al DB");
        } catch (SQLException e) {
            logger.error("Mancato inserimento di Turista " + turistaAutenticato.getNome() + " al DB", e);
        }
    }

    public void rimuoviTuristaAlDatabase(TuristaAutenticato turistaAutenticato){
        System.out.println("Rim " +turistaAutenticato.getId());
            String removeDataSQL = "DELETE FROM TURISTI " +
                                    "WHERE ID = " + turistaAutenticato.getId() + " ;";
        try (Statement statement = connection.createStatement()) {
            statement.executeUpdate(removeDataSQL);
            logger.debug("Turista: " + turistaAutenticato.getNome() + "rimosso dal DB");
    }
        catch (SQLException e) {
            logger.error("Mancata rimozione di Turista " + turistaAutenticato.getNome() + " dal DB", e);
        }
    }
}


