package ids.unicam.models.contenuti;

import ids.unicam.Comune;
import ids.unicam.models.Tempo;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static ids.unicam.Main.logger;


@Entity
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

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public Itinerario() {
    }

    @OneToOne
    @JoinColumn(name = "nome_comune")
    private Comune comune;

    @Transient
    private Tempo scadenza;
    @ElementCollection
    private List<String> tags = new ArrayList<>();

    public void setScadenza(Tempo scadenza) {
        this.scadenza = scadenza;
    }

    public Tempo getScadenza() {
        return scadenza;
    }


    public List<String> getTags() {
        return tags;
    }

    public void setTags(List<String> tags) {
        this.tags = tags;
    }

    public void aggiungiTag(String tag) {
        tags.add(tag);
    }

    public int getNumeroTappe() {
        return percorso.size();
    }

    public String getNome() {
        return nome;
    }

    public Comune getNomeComune(){
        return comune;
    }


    public List<PuntoInteresse> getPercorso() {
        return percorso;
    }

    public Itinerario(Comune comune, String nome, PuntoInteresse... puntiInteresse) {
        this.nome = nome;
        this.comune = comune;

        for (PuntoInteresse puntoInteresse : puntiInteresse) {
            if (!comune.verificaCoordinateComune(puntoInteresse.getPt()) || !puntoInteresse.getStato().asBoolean()) {
                logger.error("Non si possono creare Itinerari con punti non approvati");
                return;
                //TODO
            }
        }
        logger.debug("Creato Itinerario " + nome);
        percorso.addAll(Arrays.stream(puntiInteresse).toList());
    }
}

