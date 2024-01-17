package ids.unicam.models;

import ids.unicam.controller.ContestController;
import ids.unicam.models.contenuti.Contest;

import java.util.Set;

public class Invito {
    private final Contest contest;
    private final String idTurista;

    public Invito(Contest contest, String idTurista) {
        this.contest = contest;
        this.idTurista = idTurista;
    }
    public boolean isValid() {
        if (contest.isOpen())
            return false;

        Set<Contest> c1 = contest.getAuthor().getComune().getContestController().getContestByTurist(idTurista);
        return c1.contains(contest);
    }

    public Contest getContest() {
        return contest;
    }
}

