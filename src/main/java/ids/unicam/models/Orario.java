package ids.unicam.models;

import ids.unicam.utilites.DayOfWeek;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Embeddable;
import jakarta.persistence.Embedded;
import jakarta.persistence.OneToMany;
import org.springframework.stereotype.Component;

import java.time.LocalTime;
import java.util.HashMap;
import java.util.Map;

@Component
public class Orario {
    @ElementCollection
    private Map<DayOfWeek, OrarioApertura> hoursMap;

    public Orario() {
        hoursMap = new HashMap<>();
        for (DayOfWeek day : DayOfWeek.values()) {
            hoursMap.put(day, null);
        }
    }

    public void setOrarioApertura(DayOfWeek day, LocalTime openingTime, LocalTime closingTime) {
        OrarioApertura hours = new OrarioApertura(openingTime, closingTime);
        hoursMap.put(day, hours);
    }

    public OrarioApertura getOrarioApertura(DayOfWeek day) {
        return hoursMap.get(day);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        for (int i = 1; i < 7; i++) {
            DayOfWeek current = DayOfWeek.asDayOfWeek(i);
            builder.append(current.name()).append(" ").append(hoursMap.get(current) == null ? "Chiuso" : hoursMap.get(current)).append("\n");
        }
        return builder.toString();
    }

    @Embeddable
    public static class OrarioApertura {

        private LocalTime openingTime = null;
        private LocalTime closingTime = null;

        public OrarioApertura(LocalTime openingTime, LocalTime closingTime) {
            this.openingTime = openingTime;
            this.closingTime = closingTime;
        }

        public OrarioApertura() {

        }

        public LocalTime getOrarioApertura() {
            return openingTime;

        }

        public LocalTime getOrarioChiusura() {
            return closingTime;
        }

        @Override
        public String toString() {
            return "Apertura: " + openingTime + "\t" + "Chiusura: " + closingTime;
        }
    }
}