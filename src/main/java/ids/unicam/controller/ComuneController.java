package ids.unicam.controller;

import ids.unicam.Service.ComuneService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.Comune;
import ids.unicam.models.DTO.ComuneDTO;
import ids.unicam.models.attori.Ruolo;
import ids.unicam.models.attori.TuristaAutenticato;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;


@RestController
@RequestMapping("/comune")
public class ComuneController implements ControllerBase<ComuneDTO, String> {

    private final ComuneService comuneService;
    private final GestorePiattaformaService gestorePiattaformaService;

    public ComuneController(ComuneService comuneService, GestorePiattaformaService gestorePiattaformaService) {
        this.comuneService = comuneService;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }

    @Override
    @GetMapping("/getAll")
    @Operation(summary = "Elenco dei comuni",
            description = "Un elenco dei comuni che sono salvati nel database.")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(comuneService.findAll());
    }


    @Override
    @GetMapping("/{nomeComune}")
    @Operation(summary = "Comune dall'identificatore univoco 'id'",
            description = "Comune dall'identificatore univoco 'id' salvato nel database.")
    public ResponseEntity<?> getById(
            @Parameter(description = "nome del comune") @PathVariable String nomeComune) {
        try {
            return ResponseEntity.ok(comuneService.getComuneByNome(nomeComune));
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @Override
    @PostMapping("/crea")
    @Operation(summary = "Creazione di un nuovo comune",
            description = "Crea un nuovo comune.")
    public ResponseEntity<?> create(@RequestParam ComuneDTO comuneDTO) {
        try {
            return ResponseEntity.ok(comuneService.creaComune(new Comune(comuneDTO)));
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }
    }


    @DeleteMapping("/{nomeComune}")
    @Operation(summary = "Elimina comune",
            description = "Eliminazione di un comune dal nome univoco.")
    public ResponseEntity<?> delete(
            @Parameter(description = "nome del comune") @PathVariable String nomeComune) {
        comuneService.deleteById(nomeComune);
        return ResponseEntity.ok("Il comune '" + nomeComune + "' e' stato eliminato.");
    }

    @PutMapping("/cambioRuolo/{username}/{ruolo}")
    @Operation(summary = "Cambia ruolo di un utente",
            description = "Richiesta di cambio ruolo di un utente.")
    public ResponseEntity<?> cambiaRuolo(
            @Parameter(description = "username dell'utente a cui si vuole cambiare ruolo") @PathVariable String username,
            @Parameter(description = "scelta del nuovo ruolo da assegnare") @PathVariable Ruolo ruolo) {
        try {
            TuristaAutenticato nuovo = gestorePiattaformaService.cambiaRuolo(username, ruolo);
            return new ResponseEntity<>(nuovo, HttpStatus.OK);
        } catch (ConnessioneFallitaException | IllegalArgumentException | UnsupportedOperationException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }
}
