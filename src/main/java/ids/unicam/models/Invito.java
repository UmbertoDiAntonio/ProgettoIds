package ids.unicam.models;

import ids.unicam.models.attori.TuristaLoggato;
import ids.unicam.models.contenuti.Contest;

import java.util.ArrayList;


public class Invito {
    private final Contest contest;
    private final TuristaLoggato turistaLoggato;

    public Invito(Contest contest, TuristaLoggato turistaLoggato) {
        this.contest = contest;
        this.turistaLoggato = turistaLoggato;
        contest.getInviti().add(this);
    }
    public boolean isValid() {
        if (contest.isOpen())
            return false;

        ArrayList<Contest> c1 = contest.getAuthor().getComune().getContestController().getContestByTurist(turistaLoggato.getId());
        return c1.contains(contest);
    }

    public Contest getContest() {
        return contest;
    }

    public TuristaLoggato getTuristaLoggato() {
        return turistaLoggato;
    }
}

