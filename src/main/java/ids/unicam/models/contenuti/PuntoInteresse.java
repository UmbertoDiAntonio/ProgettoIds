package ids.unicam.models.contenuti;

import ids.unicam.Comune;
import ids.unicam.utilites.Punto;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;

import static ids.unicam.Main.logger;


@Entity
@Table(name = "Punti_di_Interesse")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(name = "TIPO")
public abstract class PuntoInteresse extends ContenutoGenerico {
    @Override
    public String toString() {
        return "PuntoInteresse{" +
                "nome='" + nome + '\'' +
                ", pt=" + pt +
                '}';
    }

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

    public PuntoInteresse(Comune comune, String nome, Punto pt) {
        super(comune);
        if (!comune.verificaCoordinateComune(pt)) {
            logger.error("Non si possono punti di interesse fuori dal comune");
            return;
        }
        logger.debug("Creato POI " + nome + " in " + pt);
        this.nome = nome;
        this.pt = pt;
    }

    public abstract String mostraInformazioniDettagliate();

    public abstract String mostraInformazioniGeneriche();


}
