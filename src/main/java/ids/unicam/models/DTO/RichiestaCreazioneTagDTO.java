package ids.unicam.models.DTO;


import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class RichiestaCreazioneTagDTO {

    private String valore;
    private PuntoInteresse puntoInteresse;//TODO id
}
