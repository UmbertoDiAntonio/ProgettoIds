package ids.unicam.models;

import ids.unicam.utilites.DayOfWeek;

import java.time.LocalTime;
import java.util.HashMap;
import java.util.Map;

public class Orario {
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

    public static class OrarioApertura {
        private final LocalTime openingTime;
        private final LocalTime closingTime;

        public OrarioApertura(LocalTime openingTime, LocalTime closingTime) {
            this.openingTime = openingTime;
            this.closingTime = closingTime;
        }

        public LocalTime getOpeningTime() {
            return openingTime;
        }

        public LocalTime getClosingTime() {
            return closingTime;
        }

        @Override
        public String toString() {
            return "Apertura: " + openingTime + "\t" + "Chiusura: " + closingTime;
        }
    }
}