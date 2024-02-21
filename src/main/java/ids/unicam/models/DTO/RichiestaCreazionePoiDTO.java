package ids.unicam.models.DTO;


import ids.unicam.models.Comune;
import ids.unicam.models.Punto;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.contenuti.puntiInteresse.Orario;
import ids.unicam.models.contenuti.puntiInteresse.TipologiaPuntoInteresse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.jetbrains.annotations.Nullable;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class RichiestaCreazionePoiDTO {


    private String nome;
    private Punto coordinate;
    private Orario orario;
    private TipologiaPuntoInteresse tipologiaPuntoInteresse;
    private Contributor creatore;


}
