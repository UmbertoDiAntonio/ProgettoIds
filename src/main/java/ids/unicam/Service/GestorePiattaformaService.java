package ids.unicam.Service;

import ids.unicam.models.Comune;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.DTO.RichiestaCreazioneTuristaDTO;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.Ruolo;
import ids.unicam.models.attori.TuristaAutenticato;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.GregorianCalendar;

public interface GestorePiattaformaService {
    TuristaAutenticato cambiaRuolo(RichiestaCreazioneContributorDTO contributorDTO, @NotNull Ruolo ruolo);


    TuristaAutenticato registraTurista(RichiestaCreazioneTuristaDTO turistaDTO);

    TuristaAutenticato registraContributor(RichiestaCreazioneContributorDTO contributorDTO);

}
