package ids.unicam.models.DTO;

import ids.unicam.models.attori.TuristaAutenticato;
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
    //PREFERITI?

    public TuristaAutenticatoDTO(TuristaAutenticato turistaAutenticato){
        this.nome=turistaAutenticato.getNome();
        this.cognome=turistaAutenticato.getCognome();
        this.dataNascita=turistaAutenticato.getDataNascita();
        this.password=turistaAutenticato.getPassword();
        this.username=turistaAutenticato.getUsername();

    }
}
