package ids.unicam.utilites;

import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Component;

@Component
public class Punto {
    private double latitudine = 0;
    private double longitudine = 0;
    public Punto(){}

    public double getLatitudine() {
        return latitudine;
    }

    public double getLongitudine() {
        return longitudine;
    }

    public Punto(double lat, double lon) {
        this.latitudine = lat;
        this.longitudine = lon;
    }


    public double getDistanza(@NotNull Punto pt) {
        return Math.sqrt(getDistanzaAlQuadrato(pt));
    }

    public double getDistanzaAlQuadrato(@NotNull Punto pt) {
        return Math.pow(pt.getLatitudine() - latitudine, 2) + Math.pow(pt.getLongitudine() - longitudine, 2);
    }


    @Override
    public String toString() {
        return "(" + latitudine + "," + longitudine + ")";
    }


}
