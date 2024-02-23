package ids.unicam.Service;

import ids.unicam.models.DTO.InvitoDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

import java.util.List;
import java.util.Optional;

public interface TuristaAutenticatoService {

    void accettaInvitoContest(TuristaAutenticatoDTO turistaDTO, InvitoDTO invitoDTO);

    void rimuoviPreferito(String usernameTurista, int id);

    void aggiungiPreferito(String usernameTurista, int idPunto);

    List<PuntoInteresse> findPreferiti(String usernameTurista);

    /**
     * Entra nel contest se è aperto
     *
     * @param contest il contest in cui si vuole entrare
     */
    void partecipaAlContest(Integer idContest, String usernameTurista);


    Optional<TuristaAutenticato> findTuristaByUsername(String username);

    boolean verificaPassword(String password, String username);

    boolean isUsernameUnique(String username);

    List<TuristaAutenticato> getAll();

    Optional<TuristaAutenticato> getById(String username);

    void deleteById(String username);

    public List<Notifica> visualizzaNotifiche(String usernameTurista) ;

    }
