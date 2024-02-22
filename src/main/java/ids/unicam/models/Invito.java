package ids.unicam.models;

import ids.unicam.models.DTO.InvitoDTO;
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

    public @NotNull Contest getContest() {
        return contest;
    }


    public Invito(InvitoDTO invitoDTO) {
        this.contest = invitoDTO.getContest();
        this.invitato = invitoDTO.getInvitato();
    }
}

