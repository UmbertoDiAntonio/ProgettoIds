package ids.unicam.models.contenuti;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


@Getter
@Entity
@NoArgsConstructor
@Table(name = "Itinerari")
public class Itinerario {
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "itinerari_gen")
    @SequenceGenerator(name = "itinerari_gen", sequenceName = "ITINERARI_SEQ", allocationSize = 1)
    @Column(name = "id", nullable = false)
    private int id;
    private String nome = "";
    @ManyToMany(fetch = FetchType.EAGER)
    private final List<PuntoInteresse> percorso = new ArrayList<>();
    @OneToOne
    @JoinColumn(name = "nome_comune")
    private Comune comune;

    public Itinerario(Comune comune, String nome, PuntoInteresse... puntiInteresse) {
        this.nome = nome;
        this.comune = comune;
        percorso.addAll(Arrays.stream(puntiInteresse).toList());
    }
}

