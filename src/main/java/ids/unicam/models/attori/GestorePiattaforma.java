package ids.unicam.models.attori;

import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import jakarta.persistence.Entity;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Entity
@NoArgsConstructor
@Table(name = "GESTORE_PIATTAFORMA")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class GestorePiattaforma extends TuristaAutenticato {


    public GestorePiattaforma(String username, String password) {
        super(new TuristaAutenticatoDTO("GESTORE", "GESTORE", null, password, username));
    }

    @Override
    public String toString() {
        return "GestorePiattaforma " + super.toString();
    }

    @Override
    public boolean equals(Object o) {
        return super.equals(o);
    }
}
