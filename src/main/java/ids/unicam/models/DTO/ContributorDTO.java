package ids.unicam.models.DTO;

import ids.unicam.models.Comune;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class ContributorDTO {
    private Comune comune;
    private TuristaAutenticatoDTO turistaDTO;
}
