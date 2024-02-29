package ids.unicam.Service;

import ids.unicam.exception.ContestException;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;

import java.util.List;
import java.util.Optional;

public interface AnimatoreService {

    Invito invitaContest(String usernameAnimatore, int idContest, String usernameInvitato) throws ContestException, IllegalStateException, IllegalArgumentException;

    boolean approvaMateriale(String usernameAnimatore, int idContest, int idMaterialeGenerico, boolean stato) throws UnsupportedOperationException, IllegalArgumentException;

    List<Animatore> getAll();

    void deleteByUsername(String username);

    Optional<Animatore> getByUsername(String username);

    void terminaContest(String usernameAnimatore, int idContest) throws UnsupportedOperationException, IllegalArgumentException, ContestException;

    void setVincitoreContest(String usernameAnimatore, int idContest, int idMateriale) throws ContestException;

    void annullaInvito(String usernameAnimatore, int idInvito) throws ContestException, IllegalArgumentException;
}
