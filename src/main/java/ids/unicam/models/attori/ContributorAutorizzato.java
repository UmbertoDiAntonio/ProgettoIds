package ids.unicam.models.attori;

import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.ContributorDTO;
import jakarta.persistence.Entity;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;


/**
 * Classe ContributorAutorizzati, il Contributor  Autorizzato è una figura del comune che si occupa della creazione
 * di punti di interesse e di itinerari, i punti di interesse creati dai contributor sono approvati di default.
 */
@Entity
@Table(name = "CONTRIBUTOR_AUTORIZZATI")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class ContributorAutorizzato extends Contributor {
    public ContributorAutorizzato(ContributorDTO contributorDTO) throws ConnessioneFallitaException, RuntimeException {
        super(contributorDTO);
    }

    public ContributorAutorizzato() {

    }

    @Override
    public String toString() {
        return "ContributorAutorizzato " + super.toString();
    }

    @Override
    public boolean equals(Object o) {
        return super.equals(o);
    }
}