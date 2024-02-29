package ids.unicam.controller;

import ids.unicam.Service.TuristaService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RequestMapping("/Turista")
@RestController
public class TuristaController {

    private final TuristaService turistaService;

    public TuristaController(TuristaService turistaService) {
        this.turistaService = turistaService;
    }

    @GetMapping("/{tag}")
    @Operation(summary = "Cerca Punti di Interesse in base ai Tag contenuti",
            description = "Trova tutti i punti di interesse che hanno tra i loro tag il tag cercato.")
    public ResponseEntity<?> getByTag(
            @Parameter(description = "Tag cercato") @PathVariable String tag) {
        return ResponseEntity.ok(turistaService.findByTag(tag));
    }

    @GetMapping("/report")
    @Operation(summary = "Segnala un Punto di Interesse",
            description = "Segnala un Punto di interesse a tutti i curatori del comune in cui è presente.")
    public ResponseEntity<?> report(
            @Parameter(description = "id del Punto di Interesse") @RequestParam int idPuntoInteresse,
            @Parameter(description = "Messaggio della Segnalazione") @RequestParam String messaggio) {
        try {
            turistaService.report(idPuntoInteresse, messaggio);
            return ResponseEntity.ok("Il Punto di interesse con id '" + idPuntoInteresse + "' è stato segnalato.");
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

}
