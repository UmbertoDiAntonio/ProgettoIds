package ids.unicam.controller;

import ids.unicam.Service.PoiService;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Punto;
import ids.unicam.models.contenuti.puntiInteresse.DayOfWeek;
import ids.unicam.models.contenuti.puntiInteresse.Orario;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import ids.unicam.models.contenuti.puntiInteresse.TipologiaPuntoInteresse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.time.LocalTime;

@RestController
@RequestMapping("/PuntoInteresse")
public class PuntoInteresseController {
    private final PoiService poiService;

    public PuntoInteresseController(PoiService poiService) {
        this.poiService = poiService;
    }

    @GetMapping("/getAll")
    @Operation(summary = "Elenco dei punti di interesse esistenti",
            description = "Un elenco dei punti di interesse che sono salvati nel database.")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(poiService.findActive());
    }

    @GetMapping("/getAsList")
    @Operation(summary = "Lista dei punti di interesse esistenti",
            description = "Una lista dei punti di interesse che sono salvati nel database.")
    public ResponseEntity<?> getAsList() {
        return ResponseEntity.ok(poiService.getAsList());
    }

    @GetMapping("/getAsListDetailed")
    @Operation(summary = "Lista dettagliata dei punti di interesse esistenti",
            description = "Una lista dettagliata dei punti di interesse che sono salvati nel database.")
    public ResponseEntity<?> getAsListDetailed() {
        return ResponseEntity.ok(poiService.getAsListDetailed());
    }

    @PutMapping("/setOrario")
    @Operation(summary = "Impostazione dell'orario di apertura e chiusura",
            description = "Imposta l'orario di apertura e chiusura per un giorno specifico di un punto di interesse.")
    public ResponseEntity<?> setOrario(
            @Parameter(description = "ID del punto di interesse") @RequestParam Integer idPunto,
            @Parameter(description = "Ora di apertura (0-24)") @RequestParam @Min(0) @Max(24) int oraApertura,
            @Parameter(description = "Minuti di apertura (0-60)") @RequestParam @Min(0) @Max(60) int minutiApertura,
            @Parameter(description = "Ora di chiusura (0-24) ") @RequestParam @Min(0) @Max(24) int oraChiusura,
            @Parameter(description = "Minuti di chiusura (0-60)") @RequestParam @Min(0) @Max(60) int minutiChiusura,
            @Parameter(description = "Giorno della settimana") @RequestParam DayOfWeek giorno) {
        poiService.setOrario(idPunto, new Orario.OrarioApertura(LocalTime.of(oraApertura, minutiApertura), LocalTime.of(oraChiusura, minutiChiusura)), giorno);
        return ResponseEntity.ok("");
    }


    @GetMapping("/{id}")
    @Operation(summary = "Punto di interesse dall'identificatore univoco id",
            description = "Punto di interesse dall'identificatore univoco id salvato nel database.")
    public ResponseEntity<?> getById(
            @Parameter(description = "Id del punto di interesse") @PathVariable Integer id) {
        return ResponseEntity.ok(poiService.getById(id));
    }

    @PostMapping("/crea")
    @Operation(summary = "Creazione di un nuovo punto di interesse",
            description = "Crea un nuovo punto di interesse.")
    public ResponseEntity<?> create(
            @Parameter(description = "Nome del punto di interesse") @RequestParam String nomePOI,
            @Parameter(description = "Latitudine del punto di interesse") @RequestParam double latitudine,
            @Parameter(description = "Longitudine del punto di interesse") @RequestParam double longitudine,
            @Parameter(description = "Username del creatore del punto di interesse") @RequestParam String usernameCreatore,
            @Parameter(description = "Tipologia del punto di interesse", schema = @Schema(implementation = TipologiaPuntoInteresse.class)) @RequestParam TipologiaPuntoInteresse tipologiaPuntoInteresse) {
        try {
            return ResponseEntity.ok(poiService.creaPuntoInteresse(nomePOI, new Punto(latitudine, longitudine), new Orario(), tipologiaPuntoInteresse, usernameCreatore));
        } catch (FuoriComuneException |IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/addTag")
    @Operation(summary = "Aggiunta di un tag",
            description = "Aggiunge un nuovo tag a un punto di interesse.")
    public ResponseEntity<?> aggiungiTag(
            @Parameter(description = "Nome del tag") @RequestParam String nomeTag,
            @Parameter(description = "ID del punto di interesse") @RequestParam Integer idPuntoInteresse,
            @Parameter(description = "username del Contributor") @RequestParam String usernameContributor) {
        try {
            poiService.aggiungiTag(idPuntoInteresse, new Tag(nomeTag), usernameContributor);
            return ResponseEntity.ok("Aggiunto tag '" + nomeTag + "' al punto di interesse: '" + idPuntoInteresse + "' .");
        } catch (FuoriComuneException |IllegalArgumentException |IllegalStateException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/removeTag")
    @Operation(summary = "Rimozione di un tag",
            description = "Rimozione un tag da un punto di interesse.")
    public ResponseEntity<?> rimuoviTag(
            @Parameter(description = "Nome del tag") @RequestParam String nomeTag,
            @Parameter(description = "ID del punto di interesse") @RequestParam Integer idPuntoInteresse,
            @Parameter(description = "username del Contributor") @RequestParam String usernameContributor) {
        try {
            poiService.rimuoviTag(idPuntoInteresse, new Tag(nomeTag), usernameContributor);
            return ResponseEntity.ok("Rimosso tag '" + nomeTag + "' dal punto di interesse: '" + idPuntoInteresse + "' .");
        } catch (FuoriComuneException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }


    @DeleteMapping("/{id}")
    @Operation(summary = "Elimina punto di interesse",
            description = "Elimina un punto di interesse.")
    public ResponseEntity<?> delete(
            @Parameter(description = "id del Punto di Interesse") @PathVariable Integer id) {
        poiService.deleteById(id);
        return ResponseEntity.ok("Punto Interesse: '" + id + "' eliminato");
    }

    @PutMapping("/setScadenza")
    @Operation(summary = "Modifica della scadenza",
            description = "Modifica la scadenza di un punto di interesse.")
    public ResponseEntity<?> modificaScadenza(
            @Parameter(description = "Username del contributor") @RequestParam String usernameContributor,
            @Parameter(description = "ID del punto di interesse") @RequestParam Integer idPuntoInteresse,
            @Parameter(description = "Data di scadenza nel formato YYYY-MM-DD") @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate scadenza) {
        try {
            poiService.modificaScadenza(usernameContributor, idPuntoInteresse, scadenza);
            return ResponseEntity.ok("Il contributor con id '" + usernameContributor + "' ha aggiornato la scadenza a '" + scadenza + "' del punto di interesse con id '" + idPuntoInteresse + "' .");
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }

    }


    @GetMapping("/materiali/{idPunto}")
    @Operation(summary = "Elenco dei materiali di un punto di interesse",
            description = "Un elenco dei materiali presenti in un punto di interesse.")
    public ResponseEntity<?> getMateriali(
            @Parameter(description = "id del punto di interesse") @PathVariable Integer idPunto) {
        return ResponseEntity.ok(poiService.getMaterialiPoi(idPunto));
    }
}
