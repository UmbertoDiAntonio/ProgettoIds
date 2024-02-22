package ids.unicam.models.DTO;

import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class RichiestaCreazioneInvitoDTO {

    private Contest contest;
    private TuristaAutenticato invitato;
}
