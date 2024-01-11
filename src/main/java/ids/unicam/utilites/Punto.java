package ids.unicam.utilites;

import org.jetbrains.annotations.NotNull;

public class Punto {

    private double x;
    private double y;

    public Punto(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    public void normalize() {
        double length = Math.sqrt(x * x + y * y);
        if (length != 0) {
            x /= length;
            y /= length;
        } else {
            x = 0;
            y = 0;
        }
    }




    public double getDistance(@NotNull Punto pt) {
        return Math.sqrt(getDistanceSquared(pt));
    }

    public double getDistanceSquared(@NotNull Punto pt) {
        return Math.pow(pt.getX() - x, 2) + Math.pow(pt.getY() - y, 2);
    }


    @Override
    public String toString() {
        return "(" + x + "," + y + ")";
    }

}
