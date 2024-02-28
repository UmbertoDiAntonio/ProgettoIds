package ids.unicam.models.contenuti.puntiInteresse;

import jakarta.persistence.ElementCollection;
import jakarta.persistence.Embeddable;
import jakarta.persistence.FetchType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.stereotype.Component;

import java.time.LocalTime;
import java.util.HashMap;
import java.util.Map;

@Component
public class Orario {
    @ElementCollection(fetch = FetchType.EAGER)
    private final Map<DayOfWeek, OrarioApertura> hoursMap;

    public Orario() {
        hoursMap = new HashMap<>();
        for (DayOfWeek day : DayOfWeek.values()) {
            hoursMap.put(day, null);
        }
    }

    public void setOrarioApertura(DayOfWeek day, OrarioApertura orario) {
        hoursMap.put(day, orario);
    }
    public void setOrarioApertura(DayOfWeek day, LocalTime openingTime, LocalTime closingTime) {
        OrarioApertura hours = new OrarioApertura(openingTime, closingTime);
        hoursMap.put(day, hours);
    }

    public OrarioApertura getOrarioApertura(DayOfWeek day) {
        return hoursMap.get(day);
    }
/*
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        for (int i = 1; i < 7; i++) {
            DayOfWeek current = DayOfWeek.asDayOfWeek(i);
            builder.append(current.name()).append(" ").append(hoursMap.get(current) == null ? "Chiuso" : hoursMap.get(current)).append("\n");
        }
        return builder.toString();
    }

 */

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        boolean haveCloseDay=false;
        for (int i = 1; i < 7; i++) {
            DayOfWeek current = DayOfWeek.asDayOfWeek(i);
            if(hoursMap.get(current) != null )
                builder.append(current.name()).append(" ").append(hoursMap.get(current)).append(" ");
            else
                haveCloseDay=true;
        }
        if(haveCloseDay) {
            for (int i = 1; i < 7; i++) {
                DayOfWeek current = DayOfWeek.asDayOfWeek(i);
                if (hoursMap.get(current) == null)
                    builder.append(current.name()).append(" ");
            }
            builder.append("Chiuso");
        }
        return builder.toString();
    }

    @Embeddable
    @Getter
    @Setter
    @NoArgsConstructor
    public static class OrarioApertura {
        private LocalTime openingTime = null;
        private LocalTime closingTime = null;

        public OrarioApertura(LocalTime openingTime, LocalTime closingTime) {
            this.openingTime = LocalTime.of(openingTime.getHour(),openingTime.getMinute());
            this.closingTime = LocalTime.of(closingTime.getHour(),closingTime.getMinute());
        }


        public LocalTime getOrarioApertura() {
            return openingTime;

        }

        public LocalTime getOrarioChiusura() {
            return closingTime;
        }

        @Override
        public String toString() {
            return "{Apertura: " + openingTime + "," + "Chiusura: " + closingTime+"}";
        }
    }
}