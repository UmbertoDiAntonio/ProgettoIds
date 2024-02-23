package ids.unicam.models.attori;

import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import jakarta.persistence.Entity;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;

import java.io.IOException;

@Entity
@Table(name="ANIMATORI")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class Animatore extends Contributor {
    public Animatore(RichiestaCreazioneContributorDTO contributorDTO) throws ConnessioneFallitaException,IllegalArgumentException,RuntimeException {
        super(contributorDTO);
    }


    public Animatore() {
        super();
    }

    @Override
    public String toString() {
        return "Animatore "+super.toString();
    }
}
