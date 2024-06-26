package ids.unicam.controller;

import ids.unicam.Service.CuratoreService;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.contenuti.Stato;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import jakarta.validation.constraints.Min;
import org.jetbrains.annotations.NotNull;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/Curatore")
public class CuratoreController {

    private final CuratoreService curatoreService;

    public CuratoreController(CuratoreService curatoreService) {
        this.curatoreService = curatoreService;
    }


    @Operation(summary = "Elenco degli utenti curatore",
            description = "Un elenco degli utenti con ruolo di curatore.")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(curatoreService.getAll());
    }


    @GetMapping("/{username}")
    @Operation(summary = "Curatore dall'username",
            description = "Curatore dall'identificatore univoco username salvato nel database.")
    public ResponseEntity<?> getByUsername(
            @Parameter(description = "username del curatore") @PathVariable @NotNull String username) {
        return ResponseEntity.ok(curatoreService.getByUsername(username));
    }


    @DeleteMapping("/eliminaItinerario")
    @Operation(summary = "Elimina un itinerario",
            description = "Eliminazione di un itinerario dall'id univoco.")
    public ResponseEntity<?> eliminaItinerario(
            @Parameter(description = "username del curatore") @RequestParam @NotNull String usernameCuratore,
            @Parameter(description = "id dell'itinerario da eliminare") @RequestParam @Min(0) int idItinerario) {
        try {
            curatoreService.eliminaItinerario(usernameCuratore, idItinerario);
            return ResponseEntity.ok("L'itinerario con id '" + idItinerario + "' e' stato eliminato da utente con username '" + usernameCuratore + "' .");
        } catch (FuoriComuneException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @DeleteMapping("/eliminaPuntoIntesse")
    @Operation(summary = "Elimina un punto di interesse",
            description = "Eliminazione di un punto di interesse dall'id univoco.")
    public ResponseEntity<?> eliminaPuntoInteresse(
            @Parameter(description = "username del curatore") @RequestParam @NotNull String usernameCuratore,
            @Parameter(description = "id del punto di interesse da eliminare") @RequestParam @Min(0) int idPuntoInteresse) {
        try {
            curatoreService.eliminaPuntoInteresse(usernameCuratore, idPuntoInteresse);
            return ResponseEntity.ok("Il punto di interesse con id '" + idPuntoInteresse + "' e' stato eliminato da utente con username '" + usernameCuratore + "' .");
        } catch (FuoriComuneException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @DeleteMapping("/eliminaContest")
    @Operation(summary = "Elimina un contest",
            description = "Eliminazione di un contest dall'id univoco.")
    public ResponseEntity<?> eliminaContest(
            @Parameter(description = "username del curatore") @RequestParam @NotNull String usernameCuratore,
            @Parameter(description = "id del contest da eliminare") @RequestParam @Min(0) int idContest) {
        try {
            curatoreService.eliminaContest(usernameCuratore, idContest);
            return ResponseEntity.ok("Il contest con id '" + idContest + "' e' stato eliminato da utente con username '" + usernameCuratore + "' .");
        } catch (FuoriComuneException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @DeleteMapping("/eliminaMateriale")
    @Operation(summary = "Elimina un Materiale",
            description = "Eliminazione di un Materiale dall'ID.")
    public ResponseEntity<?> eliminaMateriale(
            @Parameter(description = "Username del curatore") @RequestParam @NotNull String usernameCuratore,
            @Parameter(description = "id del materiale da eliminare") @RequestParam @Min(0) int idMateriale) {
        try {
            curatoreService.eliminaMateriale(usernameCuratore, idMateriale);
            return ResponseEntity.ok("Il materiale con id '" + idMateriale + "' e' stato eliminato da utente con username '" + usernameCuratore + "' .");
        } catch (FuoriComuneException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/valutaPuntoInteresse")
    @Operation(summary = "Valutazione di un Punto di Interesse",
            description = "Cambio dello stato di un punto di interesse dall'id univoco.")
    public ResponseEntity<?> valutaPuntoInteresse(
            @Parameter(description = "username del curatore") @RequestParam @NotNull String usernameCuratore,
            @Parameter(description = "id del punto di interesse da valutare") @RequestParam @Min(0) int idPunto,
            @Parameter(description = "approvazione o non del punto di interesse") @RequestParam @NotNull Stato stato) {
        try {
            curatoreService.valutaPuntoInteresse(usernameCuratore, idPunto, stato.asBoolean());
            return ResponseEntity.ok("Stato del punto di interesse con id " + idPunto + " cambiato in " + stato);
        } catch (FuoriComuneException | IllegalArgumentException | UnsupportedOperationException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/valutaMateriale")
    @Operation(summary = "Valutazione di un Materiale",
            description = "Cambio dello stato di un materiale caricato dall'id univoco.")
    public ResponseEntity<?> valutaMateriale(
            @Parameter(description = "username del curatore") @RequestParam @NotNull String usernameCuratore,
            @Parameter(description = "id del materiale da valutare") @RequestParam @Min(0) int idMateriale,
            @Parameter(description = "approvazione o non del materiale") @RequestParam @NotNull Stato stato) {
        try {
            curatoreService.valutaMateriale(usernameCuratore, idMateriale, stato.asBoolean());
            return ResponseEntity.ok("Lo stato del materiale con id " + idMateriale + " è stato cambiato in " + stato);
        } catch (FuoriComuneException | IllegalArgumentException | UnsupportedOperationException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }
}
