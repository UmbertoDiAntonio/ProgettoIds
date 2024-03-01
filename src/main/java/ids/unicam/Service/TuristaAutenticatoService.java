package ids.unicam.Service;

import ids.unicam.exception.ContestException;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;

import java.util.List;
import java.util.Optional;

public interface TuristaAutenticatoService {

    TuristaAutenticato save(TuristaAutenticato turistaAutenticato);

    @Transactional
    void accettaInvitoContest(String usernamUtente, int idInvito) throws IllegalArgumentException, ContestException;

    void rimuoviPreferito(String usernameTurista, int id) throws IllegalArgumentException;

    void aggiungiPreferito(String usernameTurista, PuntoInteresse puntoInteresse) throws IllegalArgumentException;

    List<PuntoInteresse> findPreferiti(String usernameTurista) throws IllegalArgumentException;


    void partecipaAlContest(int idContest, String usernameTurista) throws UnsupportedOperationException, IllegalArgumentException, ContestException;


    @Transactional
    void cancellaPartecipazioneContest(int idContest, String usernameTurista) throws IllegalArgumentException;

    boolean isUsernameUnique(String username);

    List<TuristaAutenticato> getAll();

    Optional<TuristaAutenticato> getByUsername(String username);

    void deleteByUsername(String username);

    List<Notifica> visualizzaNotifiche(String usernameTurista) throws IllegalArgumentException;

    List<Invito> getInviti(String usernameTurista);

    void deleteNotificheByUsername(String usernameTurista);
}
