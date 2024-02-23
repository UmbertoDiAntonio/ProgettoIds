package ids.unicam.models.DTO;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.GregorianCalendar;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class TuristaAutenticatoDTO {
    private String nome;
    private String cognome;
    private Date dataNascita;
    private String password;
    private String username;
    //PREFERITI?
}
