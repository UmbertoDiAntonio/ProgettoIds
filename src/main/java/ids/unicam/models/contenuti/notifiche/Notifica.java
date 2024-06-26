package ids.unicam.models.contenuti.notifiche;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Entity
@Getter
@NoArgsConstructor
public class Notifica {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "sequenza_report")
    @SequenceGenerator(name = "sequenza_report", sequenceName = "NOTIFICA_SEQ", allocationSize = 1)
    private int id = 0;

    private String usernameDestinatario;
    private String titolo;
    private String descrizione;
    private LocalDate data;

    Notifica(String titolo, String descrizione, String usernameDestinatario) {
        this.titolo = titolo;
        this.descrizione = descrizione;
        this.data = LocalDate.now();
        this.usernameDestinatario = usernameDestinatario;
    }


    @Override
    public String toString() {
        return "Notifica :" +
                "\n destinatario = " + usernameDestinatario +
                "\n titolo       = " + titolo +
                "\n descrizione  = " + descrizione +
                "\n data         = " + data;
    }
}
