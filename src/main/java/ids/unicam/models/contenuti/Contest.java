package ids.unicam.models.contenuti;

import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Entity
public class Contest extends PuntoInteresse {
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE,generator = "sequenza_contenuti")
    @SequenceGenerator(name = "sequenza_contenuti", sequenceName = "PUNTI_DI_INTERESSE_SEQ", allocationSize = 1)
    private int id;

    @Column(name = "Aperto")
    private boolean open;
    private String obiettivo="";
    @OneToOne
    private Animatore creatore =null;

    @OneToMany(fetch = FetchType.EAGER)
    private final List<TuristaAutenticato> partecipanti = new ArrayList<>();
    @ElementCollection
    private final Set<String> tags = new HashSet<>();
    private String nome=null;

    public int getId() {
        return id;
    }

    public List<TuristaAutenticato> getPartecipanti() {
        return partecipanti;
    }

    public void addTags(String tags) {
        this.tags.add(tags);
    }

    public void setId(int id) {
        this.id = id;
    }

    public Contest() {
    }

    public Animatore getCreatore() {
        return creatore;
    }

    public String getNome() {
        return nome;
    }

    public void setOpen(boolean open) {
        this.open = open;
    }

    public String getObiettivo() {
        return obiettivo;
    }

    public boolean isOpen() {
        return open;
    }

    public Contest(String nome, boolean open,  String obiettivo, Animatore creatore) {
        super(creatore.getComune(), nome, creatore.getComune().getPosizione(), TipologiaPuntoInteresse.CONTEST);
        this.open = open;
        this.obiettivo = obiettivo;
        this.creatore = creatore;
        this.nome = nome;
    }





}
