package ids.unicam.models.contenuti;

import ids.unicam.Comune;
import ids.unicam.models.Expirable;
import ids.unicam.models.Taggable;
import ids.unicam.utilites.Stato;
import jakarta.persistence.*;

import java.time.LocalDate;
import java.util.HashSet;
import java.util.Set;


@MappedSuperclass
public abstract class ContenutoGenerico implements Taggable, Expirable {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "sequenza_contenuti")
    @SequenceGenerator(name = "sequenza_contenuti", sequenceName = "PUNTI_DI_INTERESSE_SEQ", allocationSize = 1)
    private int id = 0;

    @OneToOne
    @JoinColumn(name = "nome_comune")
    private Comune comune;
    private Stato stato = Stato.NOT_APPROVED;

    @OneToMany(fetch = FetchType.EAGER)
    private Set<Tag> tags = new HashSet<>();

    private LocalDate expireDate = LocalDate.MAX;

    public boolean isExpired() {
        return LocalDate.now().isAfter(expireDate);
    }

    public void setExpireDate(LocalDate expireDate) {
        this.expireDate = expireDate;
    }

    public LocalDate getExpireDate() {
        return expireDate;
    }

    public int getId() {
        return id;
    }

    public Stato getStato() {
        return stato;
    }

    public void setStato(Stato approved) {
        this.stato = approved;
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

    public Comune getComune() {
        return comune;
    }
}
