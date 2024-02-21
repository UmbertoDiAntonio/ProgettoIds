package ids.unicam.models;

import ids.unicam.models.attori.TuristaAutenticato;

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
    @SequenceGenerator(name = "sequenza_report", sequenceName = "NOTIFICA_SEQ", allocationSize = 1)
    private int id=0;

    @OneToOne
    private TuristaAutenticato ricevente;


    private String titolo;

    private String descrizione;

    private LocalDateTime data;

    public Notifica(@NotNull String titolo, @NotNull String descrizione, TuristaAutenticato ricevente)
    {
        if (titolo.isBlank() || descrizione.isBlank())
        {
            throw new IllegalArgumentException("Titolo o descrizione sono vuoti");
        }
        this.titolo = titolo;
        this.descrizione = descrizione;
        this.data = LocalDateTime.now();
        this.ricevente = ricevente;
    }

    @Override
    public String toString() {
        return "Notifica{" +
                ", titolo='" + titolo + '\'' +
                ", descrizione='" + descrizione + '\'' +
                ", data=" + data +
                '}';
    }
}
