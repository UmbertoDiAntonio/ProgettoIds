package ids.unicam.models;

import ids.unicam.models.attori.TuristaLoggato;
import ids.unicam.models.contenuti.Contest;

import java.util.ArrayList;


public class Invito {
    private final Contest contest;
    private final TuristaLoggato turistaLoggato;

    public Contest getContest() {
        return contest;
    }

    public TuristaLoggato getTuristaLoggato() {
        return turistaLoggato;
    }

    /**
     * Crea un invito e lo aggiunge alla lista degli iniviti mandati dal contest
     * @param contest il contest da cui si sta mandando l'invito
     * @param turistaLoggato il turista da invitare
     */
    public Invito(Contest contest, TuristaLoggato turistaLoggato) {
        this.contest = contest;
        this.turistaLoggato = turistaLoggato;
        contest.getInviti().add(this);
    }

    /**
     * Verifica se l'invito è valido, controlla se il contest è solo su invito e se il turista non è già entrato
     * @return true se l'invito è valido
     */
    public boolean isValid() {//TODO test e usare
        if (contest.isOpen())
            return false;

        ArrayList<Contest> c1 = contest.getAuthor().getComune().getContestController().getContestByTurist(turistaLoggato.getId());
        return c1.contains(contest);
    }


}

