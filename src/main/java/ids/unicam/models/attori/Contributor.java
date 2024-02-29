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


    public Contributor(RichiestaCreazioneContributorDTO contributorDTO) throws ConnessioneFallitaException, RuntimeException {
        super(contributorDTO.getTuristaDTO());
        this.comune = new Comune(contributorDTO.getComune());
    }

    @Override
    public String toString() {
        return "Contributor " + getUsername() + " " +
                "(" + comune + ")" +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;

        Contributor that = (Contributor) o;

        return comune.equals(that.comune);
    }

    @Override
    public int hashCode() {
        int result = super.hashCode();
        result = 31 * result + comune.hashCode();
        return result;
    }
}
