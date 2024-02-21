package ids.unicam.models.attori;

import jakarta.persistence.Entity;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;


@Entity
@Table(name="CONTRIBUTOR_AUTORIZZATI")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class ContributorAutorizzato extends Contributor{
    public ContributorAutorizzato(Contributor contributor) {
        super(contributor.getComune(),contributor);
    }

    public ContributorAutorizzato() {

    }

    @Override
    public String toString() {
        return "ContributorAutorizzato "+super.toString();
    }
}