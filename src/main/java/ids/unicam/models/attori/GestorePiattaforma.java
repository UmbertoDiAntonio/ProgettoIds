package ids.unicam.models.attori;

import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * Classe che rappresenta il gestore della piattaforma. Il gestore è colui che gestisce l'intera piattaforma ed
 * ha la possibilità di creare un nuovo comune e soprattutto di gestire il cambio ruolo degli utenti registrati.
 */
@Getter
@Entity
@NoArgsConstructor
@Table(name = "GESTORE_PIATTAFORMA")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class GestorePiattaforma extends TuristaAutenticato {

    @OneToOne
    private static GestorePiattaforma instance;

    private GestorePiattaforma(String username, String password) {
        super(new TuristaAutenticatoDTO("GESTORE", "GESTORE", null, password, username));
    }

    /**
     * Metodo statico per ottenere l'istanza singleton del gestore della piattaforma.
     *
     * @param username Il nome utente del gestore.
     * @param password La password del gestore.
     * @return L'istanza singleton del gestore della piattaforma.
     */
    public static GestorePiattaforma getInstance(String username, String password) {
        if (instance == null) {
            instance = new GestorePiattaforma(username, password);
        }
        return instance;
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
