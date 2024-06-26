package ids.unicam.models.contenuti;

import ids.unicam.models.Comune;
import ids.unicam.models.Expirable;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.jetbrains.annotations.NotNull;

import java.time.LocalDate;
import java.util.*;


@NoArgsConstructor
@Entity
public class Contest implements Contenitore, Taggable, Expirable {
    @OneToMany(fetch = FetchType.EAGER)
    private final List<TuristaAutenticato> partecipanti = new ArrayList<>();
    @ElementCollection(fetch = FetchType.EAGER)
    private final List<String> tags = new ArrayList<>();
    @OneToMany(fetch = FetchType.EAGER)
    private final Set<MaterialeGenerico> materiali = new HashSet<>();
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "sequenza_contenuti")
    @SequenceGenerator(name = "sequenza_contenuti", sequenceName = "PUNTI_DI_INTERESSE_SEQ", allocationSize = 1)
    @Getter
    private int id = 0;
    @Setter
    @Column(name = "Aperto")
    @Getter
    private boolean open;
    @Getter
    private String obiettivo = "";
    @OneToOne
    @JoinColumn(name = "nome_comune")
    @Getter
    private Comune comune;
    @OneToOne
    @Getter
    private Animatore creatore = null;

    @Column(name = "nome")
    @Getter
    private String nomeContest = null;
    @OneToOne
    @Setter
    @Getter
    private MaterialeGenerico materialeVincitore = null;
    @Setter
    @Getter
    private LocalDate expireDate = null;

    public Contest(String nomeContest, String obiettivo, Animatore creatore, boolean open) {
        this.open = open;
        this.obiettivo = obiettivo;
        this.comune = creatore.getComune();
        this.creatore = creatore;
        this.nomeContest = nomeContest;
    }

    @Override
    public boolean isExpired() {
        if (expireDate == null)
            return false;
        return LocalDate.now().isAfter(expireDate);
    }

    @Override
    public List<String> getTags() {
        return Collections.unmodifiableList(tags);
    }

    @Override
    public void addTag(@NotNull String tag) {
        tags.add(tag);
    }

    @Override
    public void rimuoviTag(@NotNull String tag) {
        tags.remove(tag);
    }

    @Override
    public boolean haveTag(@NotNull String tag) {
        return tags.contains(tag);
    }

    @Override
    public Set<MaterialeGenerico> getMateriali() {
        return Collections.unmodifiableSet(materiali);
    }

    @Override
    public void aggiungiMateriale(@NotNull MaterialeGenerico materialeGenerico) {
        if (materialeGenerico != null) {
            materiali.add(materialeGenerico);
        }
    }

    @Override
    public void rimuoviMateriale(@NotNull MaterialeGenerico materialeGenerico) {
        materiali.remove(materialeGenerico);
    }

    /**
     * Ottieni la lista dei partecipanti del contest
     *
     * @return la lista dei partecipanti
     */
    public List<TuristaAutenticato> getPartecipanti() {
        return Collections.unmodifiableList(partecipanti);
    }

    /**
     * Aggiungi un partecipante al contest
     *
     * @param partecipante il nuovo partecipante
     */
    public void aggiungiPartecipante(@NotNull TuristaAutenticato partecipante) {
        partecipanti.add(partecipante);
    }

    /**
     * Rimuovi un partecipante
     *
     * @param partecipante il partecipante da rimuovere
     */
    public void rimuoviPartecipante(@NotNull TuristaAutenticato partecipante) {
        partecipanti.remove(partecipante);
    }
}
