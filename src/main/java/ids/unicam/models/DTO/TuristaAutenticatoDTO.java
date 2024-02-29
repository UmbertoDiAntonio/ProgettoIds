package ids.unicam.models.DTO;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class TuristaAutenticatoDTO {
    private String nome;
    private String cognome;
    private LocalDate dataNascita;
    private String password;
    private String username;
}
