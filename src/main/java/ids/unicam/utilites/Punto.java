package ids.unicam.utilites;

public class Punto {

    private double latitudine;
    private double longitudine;

    public Punto(double latitudine, double longitudine) {
        this.latitudine = latitudine;
        this.longitudine = longitudine;
    }

    public double getLatitudine() {
        return latitudine;
    }

    public double getLongitudine() {
        return longitudine;
    }

    public double getDistance(){
        //TODO
        return 0;
    }

    @Override
    public String toString() {
        return "Punto{" +
                "latitudine=" + latitudine +
                ", longitudine=" + longitudine +
                '}';
    }
}
