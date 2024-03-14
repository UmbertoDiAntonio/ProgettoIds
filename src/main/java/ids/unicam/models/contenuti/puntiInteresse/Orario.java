package ids.unicam.models.contenuti.puntiInteresse;

import jakarta.persistence.ElementCollection;
import jakarta.persistence.Embeddable;
import jakarta.persistence.FetchType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.jetbrains.annotations.NotNull;
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

    /**
     * Imposta l'orario di apertura del giorno selezionato
     *
     * @param day    il giorno
     * @param orario l'orario
     */
    public void setOrarioApertura(@NotNull DayOfWeek day,  @NotNull OrarioApertura orario) {
        hoursMap.put(day, orario);
    }

    /**
     * Imposta l'orario di apertura del giorno selezionato
     *
     * @param day         il giorno
     * @param openingTime l'orario di apertura
     * @param closingTime l'orario di chiusura
     */
    public void setOrarioApertura( @NotNull DayOfWeek day, @NotNull  LocalTime openingTime,  @NotNull LocalTime closingTime) {
        OrarioApertura hours = new OrarioApertura(openingTime, closingTime);
        hoursMap.put(day, hours);
    }

    /**
     * Ottieni l'orario di apertura del giorno indicato
     *
     * @param day il giorno
     * @return l'orario
     */
    public OrarioApertura getOrarioApertura( @NotNull DayOfWeek day) {
        return hoursMap.get(day);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        boolean haveCloseDay = false;
        for (int i = 1; i < 7; i++) {
            DayOfWeek current = DayOfWeek.asDayOfWeek(i);
            if (hoursMap.get(current) != null)
                builder.append(current.name()).append(" ").append(getOrarioApertura(current)).append(" ");
            else
                haveCloseDay = true;
        }
        if (haveCloseDay) {
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
            this.openingTime = LocalTime.of(openingTime.getHour(), openingTime.getMinute());
            this.closingTime = LocalTime.of(closingTime.getHour(), closingTime.getMinute());
        }

        /**
         * Ottieni l'orario di apertura
         *
         * @return l'orario di apertura
         */
        public LocalTime getOrarioApertura() {
            return openingTime;

        }

        /**
         * Ottieni l'orario di chiusura
         *
         * @return l'orario di chiusura
         */
        public LocalTime getOrarioChiusura() {
            return closingTime;
        }

        @Override
        public String toString() {
            return "{Apertura: " + getOrarioApertura() + "," + "Chiusura: " + getOrarioChiusura() + "}";
        }
    }
}