package ids.unicam.models.contenuti;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;


@Entity
@NoArgsConstructor
@Table(name = "Itinerari")
public class Itinerario {
    @ManyToMany(fetch = FetchType.EAGER)
    private final List<PuntoInteresse> percorso = new ArrayList<>();

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "itinerari_gen")
    @SequenceGenerator(name = "itinerari_gen", sequenceName = "ITINERARI_SEQ", allocationSize = 1)
    @Column(name = "id", nullable = false)
    @Getter
    private int id;

    @Getter
    private String nome = "";

    @OneToOne
    @JoinColumn(name = "nome_comune")
    @Getter
    private Comune comune;

    public Itinerario(String nome, Comune comune) {
        this.nome = nome;
        this.comune = comune;
    }

    /**
     * Ottieni il percorso di un itinerario
     *
     * @return una lista di punti di interesse che rappresenta le sue tappe
     */
    public List<PuntoInteresse> getPercorso() {
        return Collections.unmodifiableList(percorso);
    }

    /**
     * Aggiungi una tappa all'itinerario
     *
     * @param puntoInteresse la nuova tappa
     */
    public void aggiungiTappa(@NotNull PuntoInteresse puntoInteresse) {
        percorso.add(puntoInteresse);
    }

    /**
     * Rimuovi una tappa dall'itinerario
     *
     * @param puntoInteresse la tappa da rimuovere
     */
    public void rimuoviTappa(@NotNull PuntoInteresse puntoInteresse) {
        percorso.remove(puntoInteresse);
    }
}

