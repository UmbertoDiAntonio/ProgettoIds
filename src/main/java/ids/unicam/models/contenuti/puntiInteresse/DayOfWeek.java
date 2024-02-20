package ids.unicam.models.contenuti.puntiInteresse;

public enum DayOfWeek {
        MONDAY,
        TUESDAY,
        WEDNESDAY,
        THURSDAY,
        FRIDAY,
        SATURDAY,
        SUNDAY;



    public int asValue(){
            return switch (this){
                case MONDAY -> 1;
                case TUESDAY -> 2;
                case WEDNESDAY -> 3;
                case THURSDAY -> 4;
                case FRIDAY -> 5;
                case SATURDAY -> 6;
                case SUNDAY -> 7;
            };
        }

        public static DayOfWeek asDayOfWeek(int value){
            return switch (value){
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
