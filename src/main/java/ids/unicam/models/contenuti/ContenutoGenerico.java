package ids.unicam.models.contenuti;

import ids.unicam.models.Comune;
import ids.unicam.models.Expirable;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDate;
import java.util.HashSet;
import java.util.Set;

@Getter
@MappedSuperclass
public abstract class ContenutoGenerico implements Taggable, Expirable {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "sequenza_contenuti")
    @SequenceGenerator(name = "sequenza_contenuti", sequenceName = "PUNTI_DI_INTERESSE_SEQ", allocationSize = 1)
    private int id = 0;

    @OneToOne
    @JoinColumn(name = "nome_comune")
    private Comune comune;

    @Setter
    private Stato stato = Stato.IN_ATTESA;

    @OneToMany(fetch = FetchType.EAGER)
    private final Set<Tag> tags = new HashSet<>();

    @Setter
    private LocalDate expireDate = LocalDate.MAX;

    public boolean isExpired() {
        return LocalDate.now().isAfter(expireDate);
    }

    @Override
    public Set<Tag> getTags() {
        return tags;
    }

    @Override
    public void addTag(Tag tag) {
        this.tags.add(tag);
    }

    public ContenutoGenerico(Comune comune) {
        this.comune = comune;
    }

    public ContenutoGenerico() {
    }

    @Override
    public String toString() {
        return "ContenutoGenerico{" +
                "id=" + id +
                ", comune=" + comune +
                ", stato=" + stato +
                ", tags=" + tags +
                ", expireDate=" + expireDate +
                '}';
    }
}
