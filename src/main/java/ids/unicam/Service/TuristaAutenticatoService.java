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

    void rimuoviPreferito(String usernameTurista, int id) throws IllegalArgumentException;

    void aggiungiPreferito(String usernameTurista, PuntoInteresse puntoInteresse) throws IllegalArgumentException;

    List<PuntoInteresse> findPreferiti(String usernameTurista) throws  IllegalArgumentException;


    void partecipaAlContest(Integer idContest, String usernameTurista) throws  UnsupportedOperationException,IllegalArgumentException;


    Optional<TuristaAutenticato> findTuristaByUsername(String username);

    boolean verificaPassword(String password, String username);

    boolean isUsernameUnique(String username);

    List<TuristaAutenticato> getAll();

    Optional<TuristaAutenticato> getById(String username);

    void deleteById(String username);

    public List<Notifica> visualizzaNotifiche(String usernameTurista) throws IllegalArgumentException;

    }
