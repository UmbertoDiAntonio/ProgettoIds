package ids.unicam.Service;

import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.ContributorDTO;
import ids.unicam.models.attori.GestorePiattaforma;
import ids.unicam.models.attori.Ruolo;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.RuoloRegistrazione;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;

import java.util.Optional;

public interface GestorePiattaformaService {
    /**
     * Cambia il ruolo di un utente all'interno della piattaforma
     *
     * @param usernameGestore     username del gestore della piattaforma
     * @param usernameContributor username dell'utente di cui cambiare il ruolo
     * @param ruolo               il ruolo da impostare
     * @return l'utente col ruolo cambiato
     * @throws IllegalArgumentException      se uno dei parametri non è corretto
     * @throws ConnessioneFallitaException   se non è stato possibile stabilire una connessione con il sistema OSM
     * @throws UnsupportedOperationException se il ruolo selezionato è Turista
     */
    @Transactional
    @NotNull TuristaAutenticato cambiaRuolo(@NotNull String usernameGestore, @NotNull String usernameContributor, @NotNull Ruolo ruolo) throws IllegalArgumentException, ConnessioneFallitaException, UnsupportedOperationException;

    /**
     * Registra un nuovo utente contributor alla piattaforma
     *
     * @param contributorDTO un contributor dto
     * @param ruolo          il ruolo che il contributor avrà nella piattaforma
     * @return l'utente appena registrato
     * @throws ConnessioneFallitaException se non è stato possibile stabilire una connessione con il sistema OSM
     * @throws IllegalArgumentException    se i parametri non sono validi
     */
    @NotNull TuristaAutenticato registra(@NotNull ContributorDTO contributorDTO, @NotNull RuoloRegistrazione ruolo) throws IllegalArgumentException, ConnessioneFallitaException;

    /**
     * Crea un nuovo gestore della piattaforma
     *
     * @param username username da dare al gestore
     * @param password password del gestore
     *///TODO singleton?
    void creaGestore(@NotNull String username, @NotNull String password);

    /**
     * Ottieni il gestore con l'username cercato, se esiste
     *
     * @param username l'username da cercare
     * @return il gestore con l'username cercato se esiste, altrimenti Optional.Empty
     */
    @NotNull Optional<GestorePiattaforma> findByUsername(@NotNull String username);
}
