package ids.unicam.controller;

import ids.unicam.models.attori.TuristaAutenticato;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;


public class UtentiController {


    private final ArrayList<TuristaAutenticato> turisti = new ArrayList<>();

    public ArrayList<TuristaAutenticato> getTuristi() {
        return turisti;
    }
    /**
     * Prendi un TuristaAutenticato dal suo id
     * @param id l'id da cercare
     * @return il turista con quell'id o null se non viene trovato
     */
    public @Nullable TuristaAutenticato getTuristaDaId(long id){
        return turisti.stream().filter(turistaAutenticato -> turistaAutenticato.getId()==(id)).findFirst().orElse(null);
    }




}
