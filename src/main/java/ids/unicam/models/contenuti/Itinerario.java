package ids.unicam.models.contenuti;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

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

    public List<PuntoInteresse> getPercorso() {
        return Collections.unmodifiableList(percorso);
    }

    public void aggiungiTappa(PuntoInteresse puntoInteresse){
        percorso.add(puntoInteresse);
    }
    public void rimuoviTappa(PuntoInteresse puntoInteresse){
        percorso.remove(puntoInteresse);
    }
}

