package ids.unicam.utilites;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import org.jetbrains.annotations.NotNull;


public class Punto {

    private double latitudine=0;
    private double longitudine=0;
    
    @GeneratedValue
    private Long id;

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



    public double getDistance(@NotNull Punto pt) {
        return Math.sqrt(getDistanceSquared(pt));
    }

    public double getDistanceSquared(@NotNull Punto pt) {
        return Math.pow(pt.getLatitudine() - latitudine, 2) + Math.pow(pt.getLongitudine() - longitudine, 2);
    }


    @Override
    public String toString() {
        return "(" + latitudine + "," + longitudine + ")";
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getId() {
        return id;
    }
}
