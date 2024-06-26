package ids.unicam.models;

import ids.unicam.OSM.RichiestaOSM;
import ids.unicam.exception.ConnessioneFallitaException;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

import static ids.unicam.Main.logger;


@Entity
@Table(name = "COMUNI")
@NoArgsConstructor
public class Comune {
    @Getter
    @Id
    @Column(name = "comune")
    private String nome = "";
    @Embedded
    private Punto posizione;

    /**
     * @param nomeComune Nome del Comune
     * @throws IllegalArgumentException se il nome del comune non corrisponde a nessun comune
     * @throws RuntimeException         se non è possibile raggiungere il sistema OSM
     */
    public Comune(String nomeComune) throws ConnessioneFallitaException {
        this.posizione = RichiestaOSM.getCoordinateDaComune(nomeComune);
        if (posizione == null) {
            logger.warn("Coordinate comune nulle");
        }
        this.nome = RichiestaOSM.getComuneDaCoordinate(posizione);

        if (this.nome == null)
            throw new RuntimeException("Impossibile stabilire la connessione con il sistema OSM");

        if (!this.nome.equalsIgnoreCase(nomeComune))
            throw new IllegalArgumentException("Il nome del comune ricercato non corrisponde con nessun comune reale");
    }

    @Contract("-> new")
    public @NotNull Punto getPosizione() {
        return posizione.asClone();
    }

    /**
     * @param punto il punto geografico di cui vogliamo verificare la posizione
     * @return true se il punto è all'interno del territorio del comune
     */
    public final boolean verificaCoordinateComune(Punto punto) {
        if (punto == null)
            throw new IllegalArgumentException("il punto non puo' essere nullo");
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
        return nome + posizione;
    }
}
