package ids.unicam.controller;

import ids.unicam.models.attori.TuristaLoggato;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;

public class UtentiController {
    private static long id = 0;

    public static String generateID() {
        return String.format("%04d", id++);
    }

    private final ArrayList<TuristaLoggato> turisti = new ArrayList<>();

    public ArrayList<TuristaLoggato> getTuristi() {
        return turisti;
    }

    public @Nullable TuristaLoggato getTuristaById(String id){
        return turisti.stream().filter(turistaLoggato -> turistaLoggato.getId().equals(id)).findFirst().orElse(null);
    }


}
