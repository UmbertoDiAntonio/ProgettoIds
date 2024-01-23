package ids.unicam.utilites;

import org.jetbrains.annotations.NotNull;

public class Punto {

    private double latitudine;
    private double longitudine;

    public Punto(double lat, double lon) {
        this.latitudine = lat;
        this.longitudine = lon;
    }


    public double getLatitudine() {
        return latitudine;
    }

    public double getLongitudine() {
        return longitudine;
    }

    public void normalize() {
        double length = Math.sqrt(latitudine * latitudine + longitudine * longitudine);
        if (length != 0) {
            latitudine /= length;
            longitudine /= length;
        } else {
            latitudine = 0;
            longitudine = 0;
        }
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

}
