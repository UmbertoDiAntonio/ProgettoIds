package ids.unicam.controller;

import ids.unicam.Service.ContributorAutorizzatoService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.attori.Ruolo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/ContributorAutorizzato")
public class ContributorAutorizzatoController implements ControllerBase<RichiestaCreazioneContributorDTO, String> {

    private final ContributorAutorizzatoService contributorAutorizzatoService;
    private final GestorePiattaformaService gestorePiattaformaService;

    public ContributorAutorizzatoController(ContributorAutorizzatoService contributorAutorizzatoService, GestorePiattaformaService gestorePiattaformaService) {
        this.contributorAutorizzatoService = contributorAutorizzatoService;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }

    @Override
    @GetMapping("/getAll")
    @Operation(summary = "Elenco degli utenti contributor autorizzati",
            description = "Un elenco degli utenti contributor autorizzati che sono salvati nel database.")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(contributorAutorizzatoService.getAll());
    }

    @Override
    @GetMapping("/{username}")
    @Operation(summary = "Contributor Autorizzato dall'identificatore univoco id",
            description = "Contributor Autorizzato dall'identificatore univoco id salvato nel database.")
    public ResponseEntity<?> getById(
            @Parameter(description = "username del contributor autorizzato") @PathVariable String username) {
        return ResponseEntity.ok(contributorAutorizzatoService.getById(username));
    }

    @Override
    @Operation(summary = "Creazione di un nuovo utente contributor autorizzato",
            description = "Crea un nuovo utente contributor autorizzato.")
    public ResponseEntity<?> create(@RequestBody RichiestaCreazioneContributorDTO contributorDTO) {
        try {
            return new ResponseEntity<>(gestorePiattaformaService.registraContributor(contributorDTO, Ruolo.CONTRIBUTOR_AUTORIZZATO), HttpStatus.OK);
        } catch (ConnessioneFallitaException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @Override
    @DeleteMapping("/{username}")
    @Operation(summary = "Elimina utente contributor autorizzato",
            description = "Elimina di un utente contributor autorizzato dall'username.")
    public ResponseEntity<?> delete(
            @Parameter(description = "username del contributor autorizzato") @PathVariable String username) {
        contributorAutorizzatoService.deleteById(username);
        return ResponseEntity.ok("Utente: '" + username + "' eliminato");
    }
}