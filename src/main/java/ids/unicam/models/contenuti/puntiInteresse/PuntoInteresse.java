package ids.unicam.models.contenuti.puntiInteresse;

import ids.unicam.models.Comune;
import ids.unicam.models.DTO.PuntoInteresseDTO;
import ids.unicam.models.Expirable;
import ids.unicam.models.Punto;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.contenuti.Contenitore;
import ids.unicam.models.contenuti.ContenutoGenerico;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.jetbrains.annotations.Contract;

import java.time.LocalDate;
import java.util.*;

import static ids.unicam.Main.logger;


@NoArgsConstructor
@Entity
@Getter
@Table(name = "Punti_di_Interesse")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class PuntoInteresse implements Contenitore, Taggable, Expirable {
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "sequenza_contenuti")
    @SequenceGenerator(name = "sequenza_contenuti", sequenceName = "PUNTI_DI_INTERESSE_SEQ", allocationSize = 1)
    private int id = 0;

    @OneToOne
    @JoinColumn(name = "nome_comune")
    private Comune comune;

    @Setter
    private Stato stato = Stato.IN_ATTESA;

    @OneToMany(fetch = FetchType.EAGER)
    private final Set<Tag> tags = new HashSet<>();

    @Setter
    private LocalDate expireDate = LocalDate.MAX;

    public boolean isExpired() {
        return LocalDate.now().isAfter(expireDate);
    }

    @Override
    public Set<Tag> getTags() {
        return tags;
    }

    @Override
    public String toString() {
        return "PuntoInteresse{" +
                "orario=" + orario +
                ", nome='" + nome + '\'' +
                ", pt=" + pt +
                ", creatore=" + creatore +
                ", listaMateriali=" + listaMateriali +
                ", tipo=" + tipo +
                ", id=" + id +
                ", comune=" + comune +
                ", stato=" + stato +
                ", tags=" + tags +
                ", expireDate=" + expireDate +
                '}';
    }

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

    @Contract("-> new")
    public Punto getPt() {
        return pt.clone();
    }

    @OneToMany(fetch = FetchType.EAGER)
    private final List<MaterialeGenerico> listaMateriali = new ArrayList<>();

    @Enumerated(EnumType.STRING)
    @Getter
    private TipologiaPuntoInteresse tipo;

    public PuntoInteresse(PuntoInteresseDTO poiDTO) {
        this.comune = poiDTO.getCreatore().getComune();
        if (!poiDTO.getCreatore().getComune().verificaCoordinateComune(poiDTO.getCoordinate())) {
            logger.error("Non si possono creare punti di interesse fuori dal comune");
            throw new IllegalArgumentException("Posizione Punto di Interesse Fuori dall'area del comune");
        }
        logger.debug("Creato POI " + nome + " in " + pt);
        this.setStato(poiDTO.getCreatore() instanceof ContributorAutorizzato ? Stato.APPROVATO : Stato.IN_ATTESA);
        this.nome = poiDTO.getNome();
        this.pt = poiDTO.getCoordinate();
        this.orario = poiDTO.getOrario();
        this.tipo = poiDTO.getTipologiaPuntoInteresse();
        this.creatore = poiDTO.getCreatore();
    }


    public String mostraInformazioniDettagliate() {
        return getNome() + " " + getOrario();
    }

    public String mostraInformazioniGeneriche() {
        return getNome();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        PuntoInteresse that = (PuntoInteresse) o;

        if (!Objects.equals(orario, that.orario)) return false;
        if (!Objects.equals(nome, that.nome)) return false;
        if (!Objects.equals(pt, that.pt)) return false;
        if (!creatore.equals(that.creatore)) return false;
        return tipo == that.tipo;
    }

    @Override
    public int hashCode() {
        int result = orario != null ? orario.hashCode() : 0;
        result = 31 * result + (nome != null ? nome.hashCode() : 0);
        result = 31 * result + (pt != null ? pt.hashCode() : 0);
        result = 31 * result + creatore.hashCode();
        result = 31 * result + tipo.hashCode();
        return result;
    }

    @Override
    public void addTag(Tag tag) {
        this.getTags().add(tag);
    }

    @Override
    public List<MaterialeGenerico> getMateriali() {
        return listaMateriali;
    }

    @Override
    public void addMateriale(MaterialeGenerico materialeGenerico) {
        if (materialeGenerico != null)
            listaMateriali.add(materialeGenerico);
    }

    @Override
    public void rimuoviMateriale(MaterialeGenerico materialeGenerico) {
        listaMateriali.remove(materialeGenerico);
    }
}
