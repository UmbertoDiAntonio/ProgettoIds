package ids.unicam.models;

import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.OneToOne;

import java.util.ArrayList;


@Entity
public class Invito {
    @Id
    private long id;
    @OneToOne
    private Contest contest=null;
    @OneToOne
    private TuristaAutenticato invitato=null;


    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }


    public Invito() {

    }



    public Contest getContest() {
        return contest;
    }


    public TuristaAutenticato getInvitato() {
        return invitato;
    }

    /**
     * Crea un invito e lo aggiunge alla lista degli inviti mandati dal contest
     * @param contest il contest da cui si sta mandando l'invito
     * @param invitato il turista da invitare
     */
    public Invito(Contest contest, TuristaAutenticato invitato) {
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
        ArrayList<Contest> c1 = contest.getContestController().getContestDelTurista(invitato.getId());
        return !c1.contains(contest);
    }
}

