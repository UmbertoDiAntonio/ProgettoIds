package ids.unicam.models.DTO;

import ids.unicam.models.attori.TuristaAutenticato;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class MaterialeDTO {
    private String pathFile;
    private TuristaAutenticato creatore;
}
