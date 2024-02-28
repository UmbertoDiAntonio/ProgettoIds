package ids.unicam.controller;

import ids.unicam.Service.CuratoreService;
import ids.unicam.Service.PoiService;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Punto;
import ids.unicam.models.contenuti.puntiInteresse.DayOfWeek;
import ids.unicam.models.contenuti.puntiInteresse.Orario;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import ids.unicam.models.contenuti.puntiInteresse.TipologiaPuntoInteresse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Range;
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
    private final CuratoreService curatoreService;

    public PuntoInteresseController(PoiService poiService, CuratoreService curatoreService) {
        this.poiService = poiService;
        this.curatoreService = curatoreService;
    }

    @GetMapping("/getAll")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(poiService.findActive());
    }

    @GetMapping("/getAsList")
    public ResponseEntity<?> getAsList() {
        return ResponseEntity.ok(poiService.getAsList());
    }

    @GetMapping("/getAsListDetailed")
    public ResponseEntity<?> getAsListDetailed() {
        return ResponseEntity.ok(poiService.getAsListDetailed());
    }

    @PutMapping("/setOrario")
    @Operation(summary = "Impostazione dell'orario di apertura e chiusura",
            description = "Imposta l'orario di apertura e chiusura per un giorno specifico di un punto di interesse.")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "L'orario è stato impostato con successo.")
    })
    public ResponseEntity<?> setOrario(
            @Parameter(description = "ID del punto di interesse") @RequestParam Integer idPunto,
            @Parameter(description = "Giorno della settimana") @RequestParam DayOfWeek day,
            @Parameter(description = "Ora di apertura (0-24)") @RequestParam @Min(0) @Max(24)int oraApertura,
            @Parameter(description = "Minuti di apertura (0-60)") @RequestParam @Min(0) @Max(60) int minutiApertura,
            @Parameter(description = "Ora di chiusura (0-24) ") @RequestParam @Min(0) @Max(24) int oraChiusura,
            @Parameter(description = "Minuti di chiusura (0-60)") @RequestParam @Min(0) @Max(60)int minutiChiusura) {
        poiService.setOrario(idPunto, day, new Orario.OrarioApertura(LocalTime.of(oraApertura, minutiApertura), LocalTime.of(oraChiusura, minutiChiusura)));
        return ResponseEntity.ok("");
    }


    @GetMapping("/{id}")
    public ResponseEntity<?> getById(Integer id) {
        return ResponseEntity.ok(poiService.getById(id));
    }

    @PostMapping("/crea")
    public ResponseEntity<?> create(@RequestParam String nomePOI, @RequestParam double latitudine, @RequestParam double longitudine, @RequestParam String usernameCreatore, @RequestParam @Nullable String tag, @RequestParam TipologiaPuntoInteresse tipologiaPuntoInteresse) {
        try {
            return ResponseEntity.ok(poiService.creaPuntoInteresse(nomePOI, new Punto(latitudine, longitudine), usernameCreatore, new Tag(tag), tipologiaPuntoInteresse));
        } catch (FuoriComuneException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/addTag")
    @Operation(summary = "Aggiunta di un tag",
            description = "Aggiunge un nuovo tag a un punto di interesse.")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Il tag è stato aggiunto con successo."),
            @ApiResponse(responseCode = "400", description = "Richiesta non valida.")
    })
    public ResponseEntity<?> aggiungiTag(
            @Parameter(description = "Nome del tag") @RequestParam String nomeTag,
            @Parameter(description = "ID del punto di interesse") @RequestParam Integer idPuntoInteresse) {

        try {
            poiService.aggiungiTag(idPuntoInteresse, new Tag(nomeTag));
            return ResponseEntity.ok("Aggiunto tag '" + nomeTag + "' al punto di interesse: '" + idPuntoInteresse + "' .");
        } catch (UnsupportedOperationException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "Elimina punto di interesse",
            description = "Elimina un punto di interesse.")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Eliminato con successo")
    })
    public ResponseEntity<?> delete(
            @Parameter(description = "id del Punto di Interesse") @PathVariable Integer id) {
        poiService.deleteById(id);
        return ResponseEntity.ok("Punto Interesse: '" + id + "' eliminato");
    }

    @PutMapping("/setScadenza")
    @Operation(summary = "Modifica della scadenza",
            description = "Modifica la scadenza di un punto di interesse.")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "La scadenza è stata modificata con successo."),
            @ApiResponse(responseCode = "400", description = "Richiesta non valida.")
    })
    public ResponseEntity<?> modificaScadenza(
            @Parameter(description = "Username del contributor") @RequestParam String usernameContributor,
            @Parameter(description = "ID del punto di interesse") @RequestParam Integer idPuntoInteresse,
            @Parameter(description = "Data di scadenza nel formato YYYY-MM-DD") @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate scadenza) {
        try {
            poiService.modificaScadenza(usernameContributor, idPuntoInteresse, scadenza);
            return ResponseEntity.ok("Il contributor con id '" + usernameContributor + "' ha aggionrato la scadenza a '" + scadenza + "' del punto di interesse con id '" + idPuntoInteresse + "' .");
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }

    }

    @PutMapping("/condividi/{idPunto}")
    public ResponseEntity<?> condividi(@RequestParam String usernameCuratore, @PathVariable Integer idPunto) {
        try {
            curatoreService.condividi(usernameCuratore, idPunto);
            return ResponseEntity.ok("L'utente con id '" + usernameCuratore + "' ha condiviso il punto di interesse con id '" + usernameCuratore + "' .");
        } catch (IllegalArgumentException | UnsupportedOperationException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @GetMapping("/materiali/{idPunto}")
    public ResponseEntity<?> getMateriali(@PathVariable Integer idPunto) {
        return ResponseEntity.ok(poiService.getMaterialiPoi(idPunto));
    }
}
