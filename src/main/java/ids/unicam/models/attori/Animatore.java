package ids.unicam.models.attori;

import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.ContributorDTO;
import jakarta.persistence.Entity;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;

/**
 * Classe Animatore, l'animatore è una figura del comune che si occupa della gestione dei contest, dalla creazione agli
 * inviti a partecipare, inoltre può valutare i materiali che vengono caricati dai partecipanti nei suoi Contest, impostare
 * la data di fine del contest e il vincitore
 */
@Entity
@Table(name = "ANIMATORI")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class Animatore extends Contributor {
    public Animatore(ContributorDTO contributorDTO) throws ConnessioneFallitaException, RuntimeException {
        super(contributorDTO);
    }

    public Animatore() {
        super();
    }

    @Override
    public String toString() {
        return "Animatore " + super.toString();
    }
}
