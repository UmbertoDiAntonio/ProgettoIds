package ids.unicam.Service;

import ids.unicam.models.DTO.RichiestaCreazioneContestDTO;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;

import java.util.List;
import java.util.Optional;

public interface AnimatoreService {

   // Contest creaContest(RichiestaCreazioneContestDTO contestDTO);

    Invito invitaContest(String idAnimatore, Integer idContest, String usernameInvitato);

    boolean approvaMateriale(Animatore animatore, Contest contest, MaterialeGenerico materialeGenerico, Stato stato);

    List<Animatore> getAll();

    void deleteById(String username);

    Optional<Animatore> getById(String username);

    Animatore update(RichiestaCreazioneContributorDTO contributorDTO, String username);

}
