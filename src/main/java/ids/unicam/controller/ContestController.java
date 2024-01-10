package ids.unicam.controller;

import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.attori.TuristaLoggato;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

public class ContestController {
    private List<Materiale> waitingMaterials =new ArrayList<>();

    public static @NotNull Set<Contest> getContestByTurist(long idTurista) {
        //TODO
        return Collections.emptySet();
    }

    public List<Materiale> getWaitingMaterials() {
        return waitingMaterials;
    }
    public void invita(Contest contest, TuristaLoggato turistaLoggato){

    }
}
