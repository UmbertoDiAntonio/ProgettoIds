package ids.unicam.models.contenuti.puntiInteresse;

import ids.unicam.models.Comune;
import ids.unicam.models.Expirable;
import ids.unicam.models.Punto;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.contenuti.Contenitore;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDate;
import java.util.*;

import static ids.unicam.Main.logger;


@NoArgsConstructor
@Entity
@Getter
@Table(name = "Punti_di_Interesse")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class PuntoInteresse implements Contenitore, Taggable, Expirable {
    @ElementCollection(fetch = FetchType.EAGER)
    private final List<String> tags = new ArrayList<>();
    @OneToMany(fetch = FetchType.EAGER)
    private final Set<MaterialeGenerico> materiali = new HashSet<>();
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "sequenza_contenuti")
    @SequenceGenerator(name = "sequenza_contenuti", sequenceName = "PUNTI_DI_INTERESSE_SEQ", allocationSize = 1)
    private int id = 0;
    @OneToOne
    @JoinColumn(name = "nome_comune")
    private Comune comune;
    @Setter
    private Stato stato = Stato.IN_ATTESA;
    @Setter
    private @Nullable LocalDate expireDate = null;
    @Embedded
    @Getter
    private Orario orario;
    @Getter
    private String nome = "";
    @Embedded
    private Punto pt = null;
    @Getter
    @Setter
    @OneToOne
    private Contributor creatore = null;
    @Enumerated(EnumType.STRING)
    @Getter
    private TipologiaPuntoInteresse tipo;

    public PuntoInteresse(String nome, Punto posizione, Orario orario, TipologiaPuntoInteresse tipologiaPuntoInteresse, Contributor autore) {
        this.comune = autore.getComune();
        this.nome = nome;
        this.pt = posizione;
        this.orario = orario;
        this.tipo = tipologiaPuntoInteresse;
        this.creatore = autore;
        logger.debug("Creato POI " + nome + " in " + pt);
    }

    @Override
    public boolean isExpired() {
        if (expireDate == null)
            return false;
        return LocalDate.now().isAfter(expireDate);
    }

    @Override
    public List<String> getTags() {
        return tags;
    }

    @Override
    public String toString() {
        return "PuntoInteresse{" +
                "orario=" + orario +
                ", nome='" + nome + '\'' +
                ", pt=" + pt +
                ", creatore=" + creatore +
                ", listaMateriali=" + materiali +
                ", tipo=" + tipo +
                ", id=" + id +
                ", comune=" + comune +
                ", stato=" + stato +
                ", tags=" + tags +
                ", expireDate=" + expireDate +
                '}';
    }

    @Contract("-> new")
    public Punto getPt() {
        return pt.asClone();
    }

    /**
     * @return il punto di interesse formattato come stringa dettagliata
     */
    public String mostraInformazioniDettagliate() {
        return getNome() + " " + getPt() + " (" + comune.getNome() + ") " + getTipo() + " [" + getStato() + "] " + getOrario() + " " + getCreatore().getUsername();
    }

    /**
     * @return il punto di interesse formattato come stringa
     */
    public String mostraInformazioniGeneriche() {
        return getNome() + " [" + getStato() + "]";
    }


    @Override
    public void addTag(String tag) {
        this.getTags().add(tag);
    }

    @Override
    public void rimuoviTag(String tag) {
        tags.remove(tag);
    }

    @Override
    public boolean haveTag(String tag) {
        return tags.contains(tag);
    }

    @Override
    public Set<MaterialeGenerico> getMateriali() {
        return Collections.unmodifiableSet(materiali);
    }

    @Override
    public void aggiungiMateriale(MaterialeGenerico materialeGenerico) {
        if (materialeGenerico != null)
            materiali.add(materialeGenerico);
    }

    @Override
    public void rimuoviMateriale(MaterialeGenerico materialeGenerico) {
        materiali.remove(materialeGenerico);
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        PuntoInteresse that = (PuntoInteresse) o;

        if (id != that.id) return false;
        if (!comune.equals(that.comune)) return false;
        if (!tags.equals(that.tags)) return false;
        if (!Objects.equals(expireDate, that.expireDate)) return false;
        if (!Objects.equals(orario, that.orario)) return false;
        if (!nome.equals(that.nome)) return false;
        if (!pt.equals(that.pt)) return false;
        if (!creatore.equals(that.creatore)) return false;
        if (!materiali.equals(that.materiali)) return false;
        return tipo == that.tipo;
    }

    @Override
    public int hashCode() {
        int result = id;
        result = 31 * result + comune.hashCode();
        result = 31 * result + tags.hashCode();
        result = 31 * result + (expireDate != null ? expireDate.hashCode() : 0);
        result = 31 * result + (orario != null ? orario.hashCode() : 0);
        result = 31 * result + nome.hashCode();
        result = 31 * result + pt.hashCode();
        result = 31 * result + creatore.hashCode();
        result = 31 * result + materiali.hashCode();
        result = 31 * result + tipo.hashCode();
        return result;
    }
}
