package ids.unicam.models;

import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.jetbrains.annotations.NotNull;

@NoArgsConstructor
@Entity
public class Invito {

    @Setter
    @Getter
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE,generator = "sequenza_inviti")
    @SequenceGenerator(name = "sequenza_inviti", sequenceName = "INVITI_SEQ", allocationSize = 1)
    private int id;

    @OneToOne
    private Contest contest=null;

    @Getter
    @OneToOne
    private TuristaAutenticato invitato=null;
    @Getter
    @Setter
    private boolean valido=true;

    public @NotNull Contest getContest() {
        return contest;
    }


    public Invito(Contest contest, TuristaAutenticato invitato) {
        this.contest = contest;
        this.invitato = invitato;
    }
}

