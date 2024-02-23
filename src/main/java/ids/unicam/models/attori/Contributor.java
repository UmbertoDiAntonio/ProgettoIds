package ids.unicam.models.attori;

import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.Comune;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import jakarta.persistence.*;
import lombok.Getter;

@Getter
@Entity
@DiscriminatorValue("Contributor")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class Contributor extends TuristaAutenticato {
    @OneToOne
    @JoinColumn(name = "comune")
    private Comune comune = null;

    public Contributor() {

    }


    public Contributor(RichiestaCreazioneContributorDTO contributorDTO) throws ConnessioneFallitaException {
        super(contributorDTO.getTuristaDTO());
        this.comune = new Comune(contributorDTO.getComune());
    }

    @Override
    public String toString() {
        return "Contributor " + getUsername() + " " +
                "(" + comune + ")" +
                '}';
    }


}
