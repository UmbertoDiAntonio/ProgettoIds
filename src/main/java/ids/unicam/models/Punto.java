package ids.unicam.models;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Component;

@NoArgsConstructor
@Component
@Getter
public class Punto {
    private double latitudine = 0;
    private double longitudine = 0;

    public Punto(double lat, double lon) {
        this.latitudine = lat;
        this.longitudine = lon;
    }

    /**
     * Ottieni la distanza tra due punti
     *
     * @param pt il punto da cui calcolare la distanza
     * @return la distanza tra i punti
     */
    public double getDistanza(@NotNull Punto pt) {
        return Math.sqrt(getDistanzaAlQuadrato(pt));
    }

    /**
     * Ottieni la distanza tra due punti, al quadrato
     *
     * @param pt il punto da cui calcolare la distanza
     * @return la distanza tra i punti
     */
    public double getDistanzaAlQuadrato(@NotNull Punto pt) {
        return Math.pow(pt.getLatitudine() - latitudine, 2) + Math.pow(pt.getLongitudine() - longitudine, 2);
    }

    @Contract("-> new")
    public @NotNull Punto asClone() {
        return new Punto(latitudine, longitudine);
    }

    @Override
    public @NotNull String toString() {
        return "(" + latitudine + "," + longitudine + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Punto punto = (Punto) o;

        if (Double.compare(latitudine, punto.latitudine) != 0) return false;
        return Double.compare(longitudine, punto.longitudine) == 0;
    }

    @Override
    public int hashCode() {
        int result;
        long temp;
        temp = Double.doubleToLongBits(latitudine);
        result = (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(longitudine);
        result = 31 * result + (int) (temp ^ (temp >>> 32));
        return result;
    }
}
