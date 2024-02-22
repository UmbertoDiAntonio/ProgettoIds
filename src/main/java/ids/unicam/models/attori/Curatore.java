package ids.unicam.models.attori;

import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;


@Entity
@Table(name = "CURATORI")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class Curatore extends ContributorAutorizzato {
    public Curatore() {
    }

    public Curatore(RichiestaCreazioneContributorDTO contributorDTO) {
        super(contributorDTO);
    }

    @OneToMany(fetch = FetchType.EAGER)
    private final List<Contributor> osservatori = new ArrayList<>();



    public List<Contributor> getOsservatori() {
        return osservatori;
    }




    @Override
    public String toString() {
        return "Curatore "+super.toString();
    }


}
