package ids.unicam.OSM;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import ids.unicam.Exception.ConnectionFailed;
import ids.unicam.utilites.Punto;
import org.jetbrains.annotations.Nullable;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;

public class OSMRequester {

    /**
     * Ottieni il nome del comune alle coordinate inviate
     * @param punto il punto di cui vogliamo avere informazioni
     * @return il nome del comune a cui appartiene il punto, o null se la connessione è fallita
     *
     */
    public static @Nullable String getComuneAt(Punto punto) throws ConnectionFailed {
        try {
            String apiUrl = buildReverseApiUrl(punto);
            String jsonResponse = makeHttpRequest(apiUrl);
            if(jsonResponse == null)
                return null;
            JsonNode jsonNode = new ObjectMapper().readTree(jsonResponse);
            return jsonNode.path("address").path(jsonNode.path("address").fieldNames().next()).asText();
        } catch (IOException e) {
            throw new ConnectionFailed("La connessione non è riuscita");
        }
    }

    /**
     * Ottieni le coordinate del centro del comune di cui si vuole ottenere l'informazione
     *
     * @param nome il nome del comune di cui stiamo cercando il centro
     * @return la latitudine e longitudine del centro del comune
     */
    public static @Nullable Punto getCentroComune(String nome) throws ConnectionFailed {
        try {
            String apiUrl = buildSearchApiUrl(nome);
            String jsonResponse = makeHttpRequest(apiUrl);
            if(jsonResponse == null)
                return null;
            ArrayNode jsonArray = (ArrayNode) new ObjectMapper().readTree(jsonResponse);
            if (jsonArray.isEmpty()) {
                System.out.println("Nessun elemento trovato nella risposta JSON.");
                return null;
            }
            JsonNode firstElement = jsonArray.get(0);
            double latitudine = firstElement.path("lat").asDouble();
            double longitudine = firstElement.path("lon").asDouble();

            return new Punto(latitudine, longitudine);
        } catch (IOException e) {
            throw new ConnectionFailed("La connessione non è riuscita");
        }
    }

    private static String buildReverseApiUrl(Punto punto) {
        return "https://nominatim.openstreetmap.org/reverse?lat=" +
                punto.getLatitudine() + "&lon=" + punto.getLongitudine() +
                "&zoom=10&format=jsonv2";
    }

    private static String buildSearchApiUrl(String nome) {
        return "https://nominatim.openstreetmap.org/search?q=" +
                nome + "&format=jsonv2";
    }

    private static @Nullable String makeHttpRequest(String apiUrl) throws IOException {
        try (BufferedReader reader = getApiResponseReader(apiUrl)) {
            StringBuilder response = new StringBuilder();
            String line;

            if(reader == null)
                return null;
            while ((line = reader.readLine()) != null) {
                response.append(line);
            }

            return response.toString();
        }
    }

    private static BufferedReader getApiResponseReader(String apiUrl) throws IOException {
        URL url = URI.create(apiUrl).toURL();
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setRequestMethod("GET");

        int responseCode = connection.getResponseCode();

        if (responseCode != 200) {
            return null;
        }

        return new BufferedReader(new InputStreamReader(connection.getInputStream()));
    }
}
