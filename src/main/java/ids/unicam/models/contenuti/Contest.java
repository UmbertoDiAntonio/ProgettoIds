package ids.unicam.models.contenuti;

import ids.unicam.models.Comune;
import ids.unicam.models.Expirable;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Getter
@NoArgsConstructor
@Entity
public class Contest implements Contenitore, Taggable, Expirable {
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "sequenza_contenuti")
    @SequenceGenerator(name = "sequenza_contenuti", sequenceName = "PUNTI_DI_INTERESSE_SEQ", allocationSize = 1)
    private int id = 0;

    @Setter
    @Column(name = "Aperto")
    private boolean open;

    private String obiettivo="";

    @OneToOne
    @JoinColumn(name = "nome_comune")
    private Comune comune;

    @OneToOne
    private Animatore creatore =null;

    @OneToMany(fetch = FetchType.EAGER)
    private final List<TuristaAutenticato> partecipanti = new ArrayList<>();

    @Setter
    private LocalDate expireDate = LocalDate.MAX;

    @OneToMany (fetch = FetchType.EAGER)
    private final Set<Tag> tags = new HashSet<>();

    @Column(name = "nome")
    private String nomeContest =null;

    @OneToMany
    private final List<MaterialeGenerico> listaMateriali = new ArrayList<>();

    @OneToOne
    @Setter
    private MaterialeGenerico materialeVincitore = null;

    public Contest(String nomeContest, boolean open, String obiettivo, Animatore creatore) {
        this.open = open;
        this.obiettivo = obiettivo;
        this.comune = creatore.getComune();
        this.creatore = creatore;
        this.nomeContest = nomeContest;
    }

    @Override
    public boolean isExpired() {
        return LocalDate.now().isAfter(expireDate);
    }

    @Override
    public Set<Tag> getTags() {
        return tags;
    }

    @Override
    public void addTag(Tag tag) {
        tags.add(tag);
    }

    @Override
    public List<MaterialeGenerico> getMateriali() {
        return listaMateriali;
    }

    @Override
    public void addMateriale(MaterialeGenerico materialeGenerico) {
        if(materialeGenerico != null)
            listaMateriali.add(materialeGenerico);
    }

    @Override
    public void rimuoviMateriale(MaterialeGenerico materialeGenerico) {
        listaMateriali.remove(materialeGenerico);
    }
}
