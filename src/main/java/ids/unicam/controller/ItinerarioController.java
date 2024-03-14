package ids.unicam.controller;

import ids.unicam.Service.ItinerarioService;
import ids.unicam.exception.FuoriComuneException;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import jakarta.validation.constraints.Min;
import org.jetbrains.annotations.NotNull;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/Itinerario")
public class ItinerarioController {
    private final ItinerarioService itinerarioService;

    public ItinerarioController(ItinerarioService itinerarioService) {
        this.itinerarioService = itinerarioService;
    }

    @GetMapping("/getAll")
    @Operation(summary = "Elenco degli Itinerari",
            description = "Un elenco degli itinerari che sono salvati nel database.")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(itinerarioService.getAll());
    }

    @GetMapping("/{id}")
    @Operation(summary = "Itinerario dall'identificatore univoco id",
            description = "Itinerario dall'identificatore univoco id salvato nel database.")
    public ResponseEntity<?> getById(
            @Parameter(description = "id dell'itinerario") @PathVariable @Min(0) int id) {
        return ResponseEntity.ok(itinerarioService.getById(id));
    }

    @GetMapping("/{nomeComune}/getItinerari")
    @Operation(summary = "Ottieni gli itinerari del Comune",
            description = "Ottieni tutti gli itinerari del Comune.")
    public ResponseEntity<?> getByComune(
            @Parameter(description = "nome del comune") @PathVariable @NotNull String nomeComune) {
        return ResponseEntity.ok(itinerarioService.find(itinerario -> itinerario.getComune().getNome().equals(nomeComune)));
    }

    @PostMapping("/crea")
    @Operation(summary = "Creazione di un nuovo itinerario",
            description = "Crea un nuovo itinerario.")
    public ResponseEntity<?> create(
            @Parameter(description = "username del creatore") @RequestParam @NotNull String usernameCreatore,
            @Parameter(description = "nome dell'itinerario") @RequestParam @NotNull String nomeItinerario) {
        try {
            return ResponseEntity.ok(itinerarioService.creaItinerario(usernameCreatore, nomeItinerario));
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }


    @PutMapping("/aggiungiTappa")
    @Operation(summary = "Aggiungi tappa ad un itinerario",
            description = "Aggiunta di una tappa ad un itinerario esistente.")
    public ResponseEntity<?> aggiungiTappaItinerario(
            @Parameter(description = "username dell'utente") @RequestParam @NotNull String usernameContributor,
            @Parameter(description = "id dell'itinerario") @RequestParam @Min(0) int idItinerario,
            @Parameter(description = "id della tappa da aggiungere") @RequestParam @Min(0) int idTappa) {
        try {
            itinerarioService.aggiungiTappa(usernameContributor, idItinerario, idTappa);
            return ResponseEntity.ok("L'utente '" + usernameContributor + "' ha aggiunto il punto di interesse '" + idTappa + "' dall'itinerario '" + idItinerario + "'.");

        } catch (IllegalArgumentException | FuoriComuneException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/rimuoviTappa")
    @Operation(summary = "Rimuovi tappa ad un itinerario",
            description = "Rimossa di una tappa ad un itinerario esistente.")
    public ResponseEntity<?> rimuoviTappaItinerario(
            @Parameter(description = "username dell'utente") @RequestParam @NotNull String usernameContributor,
            @Parameter(description = "id dell'itinerario") @RequestParam @Min(0) int idItinerario,
            @Parameter(description = "id della tappa da rimuovere") @RequestParam @Min(0) int idPunto) {
        try {
            itinerarioService.rimuoviTappa(usernameContributor, idItinerario, idPunto);
            return ResponseEntity.ok("L'utente '" + usernameContributor + "' ha eliminato il punto di interesse '" + idPunto + "' dall'itinerario '" + idItinerario + "'.");
        } catch (IllegalArgumentException | FuoriComuneException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }
}