package ids.unicam.models.DTO;

import ids.unicam.models.attori.Ruolo;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class RichiestaCreazioneContributorDTO {
    private RichiestaCreazioneComuneDTO comune;
    private TuristaAutenticatoDTO turistaDTO;
    private Ruolo ruolo;
}
