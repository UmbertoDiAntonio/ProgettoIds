package ids.unicam.models.contenuti;

import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;

@Entity
public class Contest  {
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE,generator = "sequenza_contenuti")
    @SequenceGenerator(name = "sequenza_contenuti", sequenceName = "PUNTI_DI_INTERESSE_SEQ", allocationSize = 1)
    private int id;

    @Column(name = "Aperto")
    private boolean open;
    private String obiettivo="";
    @OneToOne
    private Animatore creatore =null;

    @ManyToMany(mappedBy = "contests")
    private List<TuristaAutenticato> partecipanti = new ArrayList<>();
    @ElementCollection
    private final List<String> tags = new ArrayList<>();
    private String nome=null;

    public int getId() {
        return id;
    }


    public List<String> getTags() {
        return tags;
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


    public List<TuristaAutenticato> getPartecipanti() {
        return partecipanti;
    }

    public Contest(String nome, boolean open,  String obiettivo, Animatore creatore) {
        this.open = open;
        this.obiettivo = obiettivo;
        this.creatore = creatore;
        this.nome = nome;
    }



}
