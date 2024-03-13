package ids.unicam.models.contenuti.puntiInteresse;

import org.jetbrains.annotations.Range;

public enum DayOfWeek {
    MONDAY,
    TUESDAY,
    WEDNESDAY,
    THURSDAY,
    FRIDAY,
    SATURDAY,
    SUNDAY;

    /**
     * Effettua il casting a DayOfWeek di un intero
     *
     * @param value il valore (da 1 a 7) del giorno da convertire
     * @return il giorno
     */
    public static DayOfWeek asDayOfWeek(@Range(from = 1, to = 7) int value) {
        return switch (value) {
            case 1 -> MONDAY;
            case 2 -> TUESDAY;
            case 3 -> WEDNESDAY;
            case 4 -> THURSDAY;
            case 5 -> FRIDAY;
            case 6 -> SATURDAY;
            case 7 -> SUNDAY;
            default -> throw new IllegalArgumentException("Unexpected value: " + value);
        };
    }


}
