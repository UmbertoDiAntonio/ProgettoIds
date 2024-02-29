package ids.unicam.models.attori;

import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.ContributorDTO;
import jakarta.persistence.Entity;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;


/**
 * Classe Curatore, il Curatore è una figura del comune che si occupa della valutazione di punti di interesse e
 * di materiali.
 */
@Getter
@Entity
@NoArgsConstructor
@Table(name = "CURATORI")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class Curatore extends ContributorAutorizzato {

    public Curatore(ContributorDTO contributorDTO) throws ConnessioneFallitaException, RuntimeException {
        super(contributorDTO);
    }

    @Override
    public String toString() {
        return "Curatore " + super.toString();
    }

    @Override
    public boolean equals(Object o) {
        return super.equals(o);
    }
}
