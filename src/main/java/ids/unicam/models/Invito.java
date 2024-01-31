package ids.unicam.models;

import ids.unicam.models.attori.TuristaLoggato;
import ids.unicam.models.contenuti.Contest;

import java.util.ArrayList;


public class Invito {
    private final Contest contest;
    private final TuristaLoggato invitato;


    public Contest getContest() {
        return contest;
    }

    public TuristaLoggato getInvitato() {
        return invitato;
    }

    /**
     * Crea un invito e lo aggiunge alla lista degli inviti mandati dal contest
     * @param contest il contest da cui si sta mandando l'invito
     * @param invitato il turista da invitare
     */
    public Invito(Contest contest, TuristaLoggato invitato) {
        this.contest = contest;
        this.invitato = invitato;
        contest.getInviti().add(this);
    }

    /**
     * Verifica se l'invito è valido, controlla se il contest è solo su invito e se il turista non è già entrato
     * @return true se l'invito è valido
     */
    public boolean isValid() {
        if (contest.isOpen()){
            System.out.println("aperto");
            return false;
        }
        ArrayList<Contest> c1 = contest.getContestController().getContestByTourist(invitato.getId());
        return !c1.contains(contest);
    }


}

