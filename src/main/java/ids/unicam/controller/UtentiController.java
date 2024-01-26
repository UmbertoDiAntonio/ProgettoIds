package ids.unicam.controller;

import ids.unicam.models.attori.TuristaLoggato;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;

public class UtentiController {
    private static long id = 0;
    private final ArrayList<TuristaLoggato> turisti = new ArrayList<>();

    public ArrayList<TuristaLoggato> getTuristi() {
        return turisti;
    }

    /**
     * Genera un id progressivo
     * @return l'id formattato come stringa
     */
    public static @NotNull String generateID() {
        return String.format("%04d", id++);
    }


    /**
     * Prendi un TuristaLoggato dal suo id
     * @param id l'id da cercare
     * @return il turista con quell'id o null se non viene trovato
     */
    public @Nullable TuristaLoggato getTuristaById(String id){
        return turisti.stream().filter(turistaLoggato -> turistaLoggato.getId().equals(id)).findFirst().orElse(null);
    }


}
