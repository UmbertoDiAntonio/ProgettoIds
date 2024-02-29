package ids.unicam.models.contenuti.puntiInteresse;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@Entity
public class Tag {
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "sequenza_tag")
    @SequenceGenerator(name = "sequenza_tag", sequenceName = "TAG_SEQ", allocationSize = 1)
    private int id;

    private String valore;


    public Tag(String tag) {
        this.valore = tag;
    }

    @Override
    public String toString() {
        return valore;
    }
}
