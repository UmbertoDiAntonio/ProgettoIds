package ids.unicam.controller;

import ids.unicam.Service.ContributorAutorizzatoService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.jetbrains.annotations.NotNull;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/ContributorAutorizzato")
public class ContributorAutorizzatoController {

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
            @Parameter(description = "username del contributor autorizzato") @PathVariable @NotNull String username) {
        return ResponseEntity.ok(contributorAutorizzatoService.getByUsername(username));
    }
}