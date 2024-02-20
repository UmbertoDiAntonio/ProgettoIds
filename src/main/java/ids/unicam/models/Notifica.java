package ids.unicam.models;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.jetbrains.annotations.NotNull;

import java.time.LocalDateTime;

@Entity
@Getter
@NoArgsConstructor
public class Notifica {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "sequenza_report")
    @SequenceGenerator(name = "sequenza_report", sequenceName = "REPORT_SEQ", allocationSize = 1)
    private int id=0;
    //private Turista segnalatore;
    private String titolo;
    private String descrizione;
    private LocalDateTime data;
    public Notifica(@NotNull String titolo, @NotNull String descrizione)
    {
        if (titolo.isBlank() || descrizione.isBlank())
        {
//TODO
        }

        this.titolo = titolo;
        this.descrizione = descrizione;
        this.data = LocalDateTime.now();
    }
}
