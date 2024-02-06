package ids.unicam.DataBase;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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

    public ConnessioneDatabase getConnessioneDatabase() {
        return connessioneDatabase;
    }

    public CreazioneTabelleDatabase getCreazioneTabelleDatabase() {
        return creazioneTabelleDatabase;
    }

    public ModificaTabelleDatabase getModificaTabelleDatabase() {
        return modificaTabelleDatabase;
    }
}
