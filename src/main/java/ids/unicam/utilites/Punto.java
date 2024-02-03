package ids.unicam.utilites;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import org.jetbrains.annotations.NotNull;

@Entity
public class Punto {
    @Id
    private long id;
    private double latitudine = 0;
    private double longitudine = 0;

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public Punto() {
    }

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
