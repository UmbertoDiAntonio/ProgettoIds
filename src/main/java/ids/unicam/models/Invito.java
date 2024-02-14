package ids.unicam.models;

import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import jakarta.persistence.*;
import org.jetbrains.annotations.NotNull;


@Entity
public class Invito {
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE,generator = "sequenza_inviti")
    @SequenceGenerator(name = "sequenza_inviti", sequenceName = "INVITI_SEQ", allocationSize = 1)
    private int id;
    @OneToOne
    private Contest contest=null;
    @OneToOne
    private TuristaAutenticato invitato=null;


    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }


    public Invito() {

    }



    public @NotNull Contest getContest() {
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
    }
}

