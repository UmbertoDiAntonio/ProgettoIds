package ids.unicam.models.DTO;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class RichiestaCreazioneContributorDTO {
    private ComuneDTO comune;
    private TuristaAutenticatoDTO turistaDTO;
}
