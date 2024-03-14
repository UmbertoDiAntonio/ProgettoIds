package ids.unicam.controller;

import ids.unicam.Service.ContributorService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.ContributorDTO;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.RuoloRegistrazione;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import jakarta.validation.constraints.Min;
import org.jetbrains.annotations.NotNull;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/Contributor")
public class ContributorController {

    private final ContributorService contributorService;
    private final GestorePiattaformaService gestorePiattaformaService;
    private final TuristaAutenticatoController turistaAutenticatoController;

    public ContributorController(ContributorService contributorService, GestorePiattaformaService gestorePiattaformaService, TuristaAutenticatoController turistaAutenticatoController) {
        this.contributorService = contributorService;
        this.gestorePiattaformaService = gestorePiattaformaService;
        this.turistaAutenticatoController = turistaAutenticatoController;
    }


    @GetMapping("/getAll")
    @Operation(summary = "Elenco degli utenti contributor",
            description = "Un elenco degli utenti contributor che sono salvati nel database.")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(contributorService.getAll());
    }


    @GetMapping("/{username}")
    @Operation(summary = "Contributor dall'identificatore univoco 'username'",
            description = "Contributor dall'identificatore univoco 'username' salvato nel database.")
    public ResponseEntity<?> getByUsername(
            @Parameter(description = "username del contributor") @PathVariable @NotNull String username) {
        return ResponseEntity.ok(contributorService.getByUsername(username));
    }


    @PostMapping("/crea")
    @Operation(summary = "Creazione di un nuovo utente contributor",
            description = "Crea un nuovo utente con ruolo di contributor.")
    public ResponseEntity<?> create(@RequestBody @NotNull ContributorDTO contributorDTO) {
        try {
            TuristaAutenticato contributor = gestorePiattaformaService.registra(contributorDTO, RuoloRegistrazione.CONTRIBUTOR);
            return new ResponseEntity<>(contributor, HttpStatus.OK);
        } catch (IllegalArgumentException | ConnessioneFallitaException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }


    @DeleteMapping("/{username}")
    @Operation(summary = "Elimina contributor",
            description = "Elimina di un utente contributor dall'username.")
    public ResponseEntity<?> delete(
            @Parameter(description = "username del contributor") @PathVariable @NotNull String username) {
        contributorService.deleteByUsername(username);
        return ResponseEntity.ok("Utente: '" + username + "' eliminato");
    }


    @PutMapping("/accettaInvito")
    @Operation(summary = "Accetta invito per partecipare ad un contest",
            description = "Accetta invito per partecipare ad un contest da parte di un utente animatore.")
    public ResponseEntity<?> accettaInvito(
            @Parameter(description = "username dell'invitato") @RequestParam @NotNull String usernameInvitato,
            @Parameter(description = "id del contest a cui partecipare") @RequestParam @Min(0) int idInvito) {
        return turistaAutenticatoController.accettaInvito(usernameInvitato, idInvito);
    }


}
