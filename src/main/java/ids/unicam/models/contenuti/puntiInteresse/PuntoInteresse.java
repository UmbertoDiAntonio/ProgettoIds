package ids.unicam.models.contenuti.puntiInteresse;

import ids.unicam.models.Comune;
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
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static ids.unicam.Main.logger;


@NoArgsConstructor
@Entity
@Table(name = "Punti_di_Interesse")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class PuntoInteresse extends ContenutoGenerico implements Contenitore, Taggable, Expirable {
    @Override
    public String toString() {
        return "PuntoInteresse{" +
                "nome='" + nome + '\'' +
                ", pt=" + pt +
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

    public PuntoInteresse(Comune comune, String nome, Punto pt, TipologiaPuntoInteresse tipologiaPuntoInteresse, @NotNull Contributor creatore) {
        super(comune);
        if (!comune.verificaCoordinateComune(pt)) {
            logger.error("Non si possono creare punti di interesse fuori dal comune2");
            throw new IllegalArgumentException("Posizione Punto di Interesse Fuori dall'area del comune");
        }
        if (!creatore.getComune().equals(getComune())) {
            logger.error(creatore.getNome() + " non puo' creare punti di interesse fuori dal suo comune");
            throw new UnsupportedOperationException(creatore + " non può creare punti di interesse fuori dal suo comune");
        }
        logger.debug("Creato POI " + nome + " in " + pt);
        this.setStato(Stato.toStatus(creatore instanceof ContributorAutorizzato));
        this.nome = nome;
        this.pt = pt;
        this.orario = new Orario();
        this.tipo = tipologiaPuntoInteresse;
        this.creatore = creatore;
    }

    public PuntoInteresse(Comune comune, String nome, Punto pt,Orario orario, TipologiaPuntoInteresse tipologiaPuntoInteresse,
                          @NotNull Contributor creatore) {
        super(comune);
        System.out.println("DENTRO "+pt);
        if (!comune.verificaCoordinateComune(pt)) {
            logger.error("Non si possono creare punti di interesse fuori dal comune1");
            return;
        }
        logger.debug("Creato POI " + nome + " in " + pt);
        this.nome = nome;
        this.pt = pt;
        this.orario = orario;
        this.tipo = tipologiaPuntoInteresse;
        this.creatore = creatore;
        System.out.println("ESCO");
    }

    public String mostraInformazioniDettagliate(){
        return getNome() + " " + getOrario();
    }

    public String mostraInformazioniGeneriche(){
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
        if(materialeGenerico != null)
            listaMateriali.add(materialeGenerico);
    }

    @Override
    public void rimuoviMateriale(MaterialeGenerico materialeGenerico) {
        listaMateriali.remove(materialeGenerico);
    }
}
