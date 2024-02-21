package ids.unicam.models.DTO;


import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class RichiestaCreazioneItinerarioDTO {

    private Comune comune;
    private String nome;
    private PuntoInteresse[] puntoInteresse;
}
