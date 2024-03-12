package ids.unicam.controller;

import ids.unicam.Service.ContributorAutorizzatoService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.ContributorDTO;
import ids.unicam.models.attori.Ruolo;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.RuoloRegistrazione;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/ContributorAutorizzato")
public class ContributorAutorizzatoController  {

    private final ContributorAutorizzatoService contributorAutorizzatoService;

    public ContributorAutorizzatoController(ContributorAutorizzatoService contributorAutorizzatoService) {
        this.contributorAutorizzatoService = contributorAutorizzatoService;
    }


    @GetMapping("/getAll")
    @Operation(summary = "Elenco degli utenti contributor autorizzati",
            description = "Un elenco degli utenti contributor autorizzati che sono salvati nel database.")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(contributorAutorizzatoService.getAll());
    }


    @GetMapping("/{username}")
    @Operation(summary = "Contributor Autorizzato dall'identificatore univoco id",
            description = "Contributor Autorizzato dall'identificatore univoco id salvato nel database.")
    public ResponseEntity<?> getByUsername(
            @Parameter(description = "username del contributor autorizzato") @PathVariable String username) {
        return ResponseEntity.ok(contributorAutorizzatoService.getByUsername(username));
    }
}