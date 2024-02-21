package ids.unicam.models;

/**
 * Interfaccia usata da oggetti che hanno una validità limitata
 */
public interface Expirable {
    /**
     * Metodo per sapere se la validità è terminata
     * @return true se la validità è terminata
     */
    boolean isExpired();
}
