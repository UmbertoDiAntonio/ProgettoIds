package ids.unicam.Service;

import ids.unicam.models.Invito;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;

import java.util.List;
import java.util.Optional;

public interface TuristaAutenticatoService {

    void accettaInvitoContest(TuristaAutenticato turistaAutenticato, Invito invito);

    void rimuoviPreferito(String usernameTurista, int id) throws IllegalArgumentException;

    void aggiungiPreferito(String usernameTurista, PuntoInteresse puntoInteresse) throws IllegalArgumentException;

    List<PuntoInteresse> findPreferiti(String usernameTurista) throws  IllegalArgumentException;


    void partecipaAlContest(Integer idContest, String usernameTurista) throws  UnsupportedOperationException,IllegalArgumentException;


    @Transactional
    void cancellaPartecipazioneContest(Integer idContest, String usernameTurista) throws IllegalArgumentException;

    Optional<TuristaAutenticato> findTuristaByUsername(String username);

    boolean isUsernameUnique(String username);

    List<TuristaAutenticato> getAll();

    Optional<TuristaAutenticato> getById(String username);

    void deleteById(String username);

    List<Notifica> visualizzaNotifiche(String usernameTurista) throws IllegalArgumentException;

    List<Invito> getInviti(String usernameTurista);
}
