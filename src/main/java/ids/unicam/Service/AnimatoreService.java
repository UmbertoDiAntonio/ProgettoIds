package ids.unicam.Service;

import ids.unicam.exception.ContestException;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;

import java.util.List;
import java.util.Optional;

public interface AnimatoreService {


    Invito invitaContest(String idAnimatore, Integer idContest, String usernameInvitato) throws ContestException,IllegalStateException,IllegalArgumentException;

    boolean approvaMateriale(String usernameAnimatore,Integer idContest, Integer idMaterialeGenerico, boolean stato) throws UnsupportedOperationException,IllegalArgumentException ;

    List<Animatore> getAll();

    void deleteById(String username);

    Optional<Animatore> getById(String username);


}
