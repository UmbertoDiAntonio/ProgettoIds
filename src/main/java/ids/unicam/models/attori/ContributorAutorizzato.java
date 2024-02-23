package ids.unicam.models.attori;

import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import jakarta.persistence.Entity;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;


@Entity
@Table(name="CONTRIBUTOR_AUTORIZZATI")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class ContributorAutorizzato extends Contributor{
    public ContributorAutorizzato(RichiestaCreazioneContributorDTO contributorDTO) throws ConnessioneFallitaException {
        super(contributorDTO);
    }

    public ContributorAutorizzato() {

    }

    @Override
    public String toString() {
        return "ContributorAutorizzato "+super.toString();
    }
}