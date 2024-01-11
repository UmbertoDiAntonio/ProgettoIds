package ids.unicam;

import ids.unicam.utilites.Punto;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class JUnitTest {
    @Test
    void testCoordinate() {
        {
            Punto first = new Punto(0, 0);
            Punto second = new Punto(10, 0);
            assertEquals(10, first.getDistance(second));
            assertEquals(100, first.getDistanceSquared(second));
        }
    }
    }

}
