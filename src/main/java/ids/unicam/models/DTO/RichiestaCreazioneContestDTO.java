package ids.unicam.models.DTO;

import ids.unicam.models.attori.Animatore;
import jakarta.persistence.Access;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class RichiestaCreazioneContestDTO {
    private String nomeContest;
    private String obiettivo;
    private Animatore creatore;
    private boolean open;
}
