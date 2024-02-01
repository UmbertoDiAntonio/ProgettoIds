package ids.unicam.controller;

import ids.unicam.models.attori.TuristaLoggato;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.OneToMany;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;

@Entity
public class UtentiController {


    private final ArrayList<TuristaLoggato> turisti = new ArrayList<>();
    private Long id;

    private static long _id;
    @OneToMany
    public ArrayList<TuristaLoggato> getTuristi() {
        return turisti;
    }

    /**
     * Genera un id progressivo
     * @return l'id formattato come stringa
     */
    public static @NotNull String generateID() {
        return String.format("%04d", _id++);
    }


    /**
     * Prendi un TuristaLoggato dal suo id
     * @param id l'id da cercare
     * @return il turista con quell'id o null se non viene trovato
     */
    public @Nullable TuristaLoggato getTuristaById(String id){
        return turisti.stream().filter(turistaLoggato -> turistaLoggato.getId().equals(id)).findFirst().orElse(null);
    }


    public void setId(Long id) {
        this.id = id;
    }

    @Id
    @GeneratedValue
    public Long getId() {
        return id;
    }
}
