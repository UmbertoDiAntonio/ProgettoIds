package ids.unicam.models.users.organizzazioneComune;

import jakarta.persistence.Entity;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;

@Entity
@Table(name="ANIMATORI")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class Animatore extends Contributor {
    public Animatore(Contributor contributor) {
        super(contributor.getComune(), contributor);
    }


    public Animatore() {
        super();
    }

}
