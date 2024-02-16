package ids.unicam.models.contenuti;

import ids.unicam.models.Comune;
import ids.unicam.models.Expirable;
import ids.unicam.models.Orario;
import ids.unicam.models.Taggable;
import ids.unicam.models.Punto;
import jakarta.persistence.*;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

import static ids.unicam.Main.logger;


@Entity
@Table(name = "Punti_di_Interesse")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class PuntoInteresse extends ContenutoGenerico implements Taggable, Expirable {
    @Override
    public String toString() {
        return "PuntoInteresse{" +
                "nome='" + nome + '\'' +
                ", pt=" + pt +
                '}';
    }

    @Embedded
    private Orario orario;

    private String nome = "";

    @Embedded
    private Punto pt = null;


    public PuntoInteresse() {
    }

    public Punto getPt() {
        return pt;
    }

    public String getNomeContest() {
        return nome;
    }
    @Enumerated(EnumType.STRING)
    private TipologiaPuntoInteresse tipo;

    public PuntoInteresse(Comune comune, String nome, Punto pt, TipologiaPuntoInteresse tipologiaPuntoInteresse) {
        super(comune);
        if (!comune.verificaCoordinateComune(pt)) {
            logger.error("Non si possono creare punti di interesse fuori dal comune");
            throw new IllegalArgumentException("Posizione Punto di Interesse Fuori dall'area del comune");
        }
        logger.debug("Creato POI " + nome + " in " + pt);
        this.nome = nome;
        this.pt = pt;
        this.orario = new Orario();
        this.tipo = tipologiaPuntoInteresse;
    }

    public PuntoInteresse(Comune comune, String nome, Punto pt,Orario orario, TipologiaPuntoInteresse tipologiaPuntoInteresse) {
        super(comune);
        if (!comune.verificaCoordinateComune(pt)) {
            logger.error("Non si possono creare punti di interesse fuori dal comune");
            return;
        }
        logger.debug("Creato POI " + nome + " in " + pt);
        this.nome = nome;
        this.pt = pt;
        this.orario = orario;
        this.tipo = tipologiaPuntoInteresse;
    }

    public String mostraInformazioniDettagliate(){
        return getNomeContest() + " " + getOrario();
    }

    public String mostraInformazioniGeneriche(){
        return getNomeContest();
    }

    public @Nullable Orario getOrario() {
        return orario;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;

        PuntoInteresse that = (PuntoInteresse) o;

        if (!Objects.equals(orario, that.orario)) return false;
        if (!Objects.equals(nome, that.nome)) return false;
        if (!Objects.equals(pt, that.pt)) return false;
        return tipo == that.tipo;
    }

    @Override
    public int hashCode() {
        int result = super.hashCode();
        result = 31 * result + (orario != null ? orario.hashCode() : 0);
        result = 31 * result + (nome != null ? nome.hashCode() : 0);
        result = 31 * result + (pt != null ? pt.hashCode() : 0);
        result = 31 * result + tipo.hashCode();
        return result;
    }

    @Override
    public void addTag(Tag tag) {
        this.getTags().add(tag);
    }
}
