package ids.unicam.models.DTO;


import ids.unicam.models.Punto;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.contenuti.puntiInteresse.Orario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
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

    public PuntoInteresseDTO(PuntoInteresse puntoInteresse) {
        this.nome = puntoInteresse.getNome();
        this.coordinate = puntoInteresse.getPt();
        this.orario = puntoInteresse.getOrario();
        this.tipologiaPuntoInteresse = puntoInteresse.getTipo();
        this.creatore = puntoInteresse.getCreatore();
    }
}
