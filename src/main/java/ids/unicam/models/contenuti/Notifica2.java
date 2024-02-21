package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaAutenticato;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Entity
@Getter
@NoArgsConstructor
public class Notifica2 {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "sequenza_report")
    @SequenceGenerator(name = "sequenza_report", sequenceName = "NOTIFICA_SEQ", allocationSize = 1)
    private int id = 0;

    @OneToOne
    private TuristaAutenticato ricevente;

    private String titolo;
    private String descrizione;
    private LocalDateTime data;

    // Costruttore privato per il Builder
    Notifica2(String titolo, String descrizione, TuristaAutenticato ricevente) {
        this.titolo = titolo;
        this.descrizione = descrizione;
        this.data = LocalDateTime.now();
        this.ricevente = ricevente;
    }

    @Override
    public String toString() {
        return "Notifica{" +
                "ricevente=" + ricevente +
                ", titolo='" + titolo + '\'' +
                ", descrizione='" + descrizione + '\'' +
                ", data=" + data +
                '}';
    }
}
