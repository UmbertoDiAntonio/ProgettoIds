package ids.unicam;

import ids.unicam.OSM.RichiestaOSM;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.utilites.Punto;
import jakarta.persistence.*;

import java.util.Objects;

import static ids.unicam.Main.logger;


@Entity
@Table(name = "COMUNI")
public class Comune {
    @Id
    @Column(name = "comune")
    private String nome = "";
    @Embedded
    private Punto posizione;


    public Comune() {

    }


    public Punto getPosizione() {
        return posizione;
    }


    public String getNome() {
        return nome;
    }

    /**
     * @param nome Nome del Comune
     * @throws IllegalArgumentException se il nome del comune non corrisponde a nessun comune
     * @throws RuntimeException         se non è possibile raggiungere il sistema OSM
     */
    public Comune(String nome) {

        //      ComuneController.getInstance().listaComuni.add(this);
        try {
            this.posizione = RichiestaOSM.getCoordinateDaComune(nome);
            if (posizione == null) {
                logger.error("Coordinate comune nulle");
            }
            this.nome = RichiestaOSM.getComuneDaCoordinate(posizione);

            if (this.nome == null)
                throw new RuntimeException("Impossibile stabilire la connessione con il sistema OSM");

            if (!this.nome.equalsIgnoreCase(nome))
                throw new IllegalArgumentException("Il nome del comune ricercato non corrisponde con nessun comune reale");

        } catch (ConnessioneFallitaException e) {
            logger.error("Connessione fallita durante il recupero delle coordinate o del nome del comune da OSM", e);
        }
    }


    public final boolean verificaCoordinateComune(Punto punto) {
        if(punto.equals(new Punto(0,0)))
            throw new RuntimeException("0,0 IMPOSSIBLE POSITION");
        String nomeComune = RichiestaOSM.getComuneDaCoordinate(punto);
        if (nomeComune != null) {
            return nomeComune.equalsIgnoreCase(getNome());
        }
        return false;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Comune comune = (Comune) o;

        if (!nome.equals(comune.nome)) return false;
        return Objects.equals(posizione, comune.posizione);
    }

    @Override
    public int hashCode() {
        int result = nome.hashCode();
        result = 31 * result + (posizione != null ? posizione.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "Comune{" +
                "nome='" + nome + '\'' +
                ", posizione=" + posizione +
                '}';
    }
}
