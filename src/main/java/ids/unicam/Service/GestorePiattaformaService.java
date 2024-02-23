package ids.unicam.Service;

import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.attori.Ruolo;
import ids.unicam.models.attori.TuristaAutenticato;
import org.jetbrains.annotations.NotNull;

public interface GestorePiattaformaService {
    TuristaAutenticato cambiaRuolo(String usernameContributor, @NotNull Ruolo ruolo);


    TuristaAutenticato registraTurista(TuristaAutenticatoDTO turistaDTO);

    TuristaAutenticato registraContributor(RichiestaCreazioneContributorDTO contributorDTO);

}
