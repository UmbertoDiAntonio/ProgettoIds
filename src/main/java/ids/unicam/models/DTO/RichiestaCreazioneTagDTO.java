package ids.unicam.models.DTO;


import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class RichiestaCreazioneTagDTO {
    private String valore;
    private PuntoInteresseDTO puntoInteresse;
}
