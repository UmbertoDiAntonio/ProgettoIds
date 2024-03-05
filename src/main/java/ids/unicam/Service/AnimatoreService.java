package ids.unicam.Service;

import ids.unicam.exception.ContestException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import jakarta.transaction.Transactional;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface AnimatoreService {

    Animatore save(Animatore animatore);

    List<Animatore> findByNomeComune(String nomeComune);

    Invito invitaContest(String usernameAnimatore, int idContest, String usernameInvitato) throws ContestException, IllegalStateException, IllegalArgumentException;

    boolean approvaMateriale(String usernameAnimatore, int idContest, int idMaterialeGenerico, boolean stato) throws UnsupportedOperationException, IllegalArgumentException;

    List<Animatore> getAll();

    void deleteByUsername(String username);

    Optional<Animatore> getByUsername(String username);

    void terminaContest(String usernameAnimatore, int idContest) throws UnsupportedOperationException, IllegalArgumentException, ContestException;

    void setVincitoreContest(String usernameAnimatore, int idContest, int idMateriale) throws ContestException;

    void annullaInvito(String usernameAnimatore, int idInvito) throws ContestException, IllegalArgumentException;

    void setFineContest(int idContest, LocalDate dataFine, String usernameAnimatore) throws FuoriComuneException;

    @Transactional
    void aggiungiTagContest(int idContest, Tag tag, String usernameAnimatore) throws ContestException;

    @Transactional
    void rimuoviTagContest(int idContest, Tag tag, String usernameAnimatore) throws ContestException;
}
