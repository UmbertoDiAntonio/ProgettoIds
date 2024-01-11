package ids.unicam.controller;

import ids.unicam.models.attori.Animatore;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.attori.TuristaLoggato;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public class ContestController {

    private final List<Materiale> waitingMaterials = new ArrayList<>();
    private static final Set<Contest> contests = new HashSet<>();

    public static @NotNull Set<Contest> getContestByTurist(long idTurista) {
        Set<Contest> result = new HashSet<>();
        contests.stream().filter(contest -> contest.
                getInvitati().
                stream().anyMatch(turistaLoggato -> Long.parseLong(turistaLoggato.getId()) == idTurista)
        ).forEach(result::add);
        return result;
    }

    public static @NotNull Set<Contest> getContestByAuthor(long idAutore) {
        Set<Contest> result = new HashSet<>();
        contests.stream().filter(contest1 -> Long.parseLong(contest1.getAuthor().getId()) == idAutore).forEach(result::add);
        return result;
    }

    public void creaContest(boolean open, String obbiettivo, Animatore animatore) {
        contests.add(new Contest(open, this, obbiettivo, animatore));
    }

    public List<Materiale> getWaitingMaterials() {
        return waitingMaterials;
    }

    public void invita(Contest contest, TuristaLoggato turistaLoggato) {
        //TODO
    }

    public void aggiungiMateriale(TuristaLoggato turista,Materiale materiale,Contest contest){
        materiale.setApproved(false);
        contest.getMaterialiContest()
                .computeIfAbsent(turista, k -> new HashSet<>())
                .add(materiale);
    }
}
