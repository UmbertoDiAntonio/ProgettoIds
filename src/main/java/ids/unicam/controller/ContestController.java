package ids.unicam.controller;

import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaLoggato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Materiale;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class ContestController {

    private final List<Materiale> waitingMaterials = new ArrayList<>();
    private final ArrayList<Contest> contests = new ArrayList<>();

    public @NotNull ArrayList<Contest> getContestByTurist(String idTurista) {
        ArrayList<Contest> result = new ArrayList<>();
        contests.stream().filter(contest -> contest.
                getPartecipanti().
                stream().anyMatch(turistaLoggato -> turistaLoggato.getId().equals(idTurista))
        ).forEach(result::add);
        return result;
    }

    public @NotNull ArrayList<Contest> getContestByAuthor(long idAutore) {
        ArrayList<Contest> result = new ArrayList<>();
        contests.stream().filter(contest1 -> Long.parseLong(contest1.getAuthor().getId()) == idAutore).forEach(result::add);
        return result;
    }

    public void creaContest(String nome, boolean open, String obbiettivo, Animatore animatore) {
        contests.add(new Contest(nome, open, this, obbiettivo, animatore));
    }

    public List<Materiale> getWaitingMaterials() {
        return waitingMaterials;
    }

    public void invita(Invito invito) {
        invito.getTuristaLoggato().getInvitiRicevuti().add(invito);
    }

    public void aggiungiMateriale(TuristaLoggato turista,Materiale materiale,Contest contest){
        materiale.setApproved(false);
        contest.getMaterialiContest()
                .computeIfAbsent(turista, k -> new HashSet<>())
                .add(materiale);
    }

    public ArrayList<Contest> getContests() {
        return contests;
    }
}
