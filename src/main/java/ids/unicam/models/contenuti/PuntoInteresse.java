package ids.unicam.models.contenuti;

import ids.unicam.Comune;
import ids.unicam.models.Orario;
import ids.unicam.utilites.Punto;
import jakarta.persistence.*;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

import static ids.unicam.Main.logger;


@Entity
@Table(name = "Punti_di_Interesse")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(name = "TIPO")
public class PuntoInteresse extends ContenutoGenerico{
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

    public String getNome() {
        return nome;
    }

    @Embedded
    private TipologiaPuntoInteresse tipo;

    public PuntoInteresse(Comune comune, String nome, Punto pt, TipologiaPuntoInteresse tipologiaPuntoInteresse) {
        super(comune);
        if (!comune.verificaCoordinateComune(pt)) {
            logger.error("Non si possono punti di interesse fuori dal comune");
            return;
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
            logger.error("Non si possono punti di interesse fuori dal comune");
            return;
        }
        logger.debug("Creato POI " + nome + " in " + pt);
        this.nome = nome;
        this.pt = pt;
        this.orario = orario;
        this.tipo = tipologiaPuntoInteresse;
    }

    public String mostraInformazioniDettagliate(){
        return getNome() + " " + getOrario();
    };

    public String mostraInformazioniGeneriche(){
        return getNome();
    };

    public @Nullable Orario getOrario() {
        return orario;
    }

}
