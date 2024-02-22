package ids.unicam.models.DTO;


import ids.unicam.models.Punto;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.contenuti.puntiInteresse.Orario;
import ids.unicam.models.contenuti.puntiInteresse.TipologiaPuntoInteresse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class PuntoInteresseDTO {
    private String nome;
    private Punto coordinate;
    private Orario orario;
    private TipologiaPuntoInteresse tipologiaPuntoInteresse;
    private Contributor creatore;
//TAG
    //ListaMateriali
    //STATO
    //EXPIRE DATE

}
