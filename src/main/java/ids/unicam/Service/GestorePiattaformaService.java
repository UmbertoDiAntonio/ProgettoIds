package ids.unicam.Service;

import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.ContributorDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.attori.GestorePiattaforma;
import ids.unicam.models.attori.Ruolo;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.RuoloRegistrazione;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;

import java.util.Optional;

public interface GestorePiattaformaService {
    @Transactional
    TuristaAutenticato cambiaRuolo(String usernameGestore, String usernameContributor, @NotNull Ruolo ruolo) throws IllegalArgumentException, ConnessioneFallitaException, UnsupportedOperationException;

    TuristaAutenticato registra(ContributorDTO contributorDTO, RuoloRegistrazione ruolo) throws IllegalArgumentException, ConnessioneFallitaException;

    void creaGestore(String username, String password);

    Optional<GestorePiattaforma> findByUsername(String username);
}
