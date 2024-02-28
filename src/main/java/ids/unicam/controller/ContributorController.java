package ids.unicam.controller;

import ids.unicam.Service.ContributorService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.attori.Ruolo;
import ids.unicam.models.attori.TuristaAutenticato;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/Contributor")
public class ContributorController implements ControllerBase<RichiestaCreazioneContributorDTO, String> {

    private final ContributorService contributorService;
    private final GestorePiattaformaService gestorePiattaformaService;
    private final TuristaAutenticatoController turistaAutenticatoController;

    public ContributorController(ContributorService contributorService, GestorePiattaformaService gestorePiattaformaService, TuristaAutenticatoController turistaAutenticatoController) {
        this.contributorService = contributorService;
        this.gestorePiattaformaService = gestorePiattaformaService;
        this.turistaAutenticatoController = turistaAutenticatoController;
    }

    @Override
    @GetMapping("/getAll")
    @Operation(summary = "Elenco degli utenti contributor",
            description = "Un elenco degli utenti contributor che sono salvati nel database.")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(contributorService.getAll());
    }


    @Override
    @GetMapping("/{username}")
    @Operation(summary = "Contributor dall'identificatore univoco 'username'",
            description = "Contributor dall'identificatore univoco 'username' salvato nel database.")
    public ResponseEntity<?> getById(
            @Parameter(description = "username del contributor") @PathVariable String username) {
        return ResponseEntity.ok(contributorService.getById(username));
    }

    @Override
    @PostMapping("/crea")
    @Operation(summary = "Creazione di un nuovo utente contributor",
            description = "Crea un nuovo utente con ruolo di contributor.")
    public ResponseEntity<?> create(@RequestBody RichiestaCreazioneContributorDTO contributorDTO) {
        try {
            TuristaAutenticato contributor = gestorePiattaformaService.registraContributor(contributorDTO, Ruolo.CONTRIBUTOR);
            return new ResponseEntity<>(contributor, HttpStatus.OK);
        } catch (IllegalArgumentException | ConnessioneFallitaException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @Override
    @DeleteMapping("/{username}")
    @Operation(summary = "Elimina contributor",
            description = "Elimina di un utente contributor dall'username.")
    public ResponseEntity<?> delete(
            @Parameter(description = "username del contributor") @PathVariable String username) {
        contributorService.deleteById(username);
        return ResponseEntity.ok("Utente: '" + username + "' eliminato");
    }


    @PutMapping("/accettaInvito")
    @Operation(summary = "Accetta invito per partecipare ad un contest",
            description = "Accetta invito per partecipare ad un contest da parte di un utente animatore.")
    public ResponseEntity<?> accettaInvito(
            @Parameter(description = "username dell'invitato") @RequestParam String usernameInvitato,
            @Parameter(description = "id del contest a cui partecipare") @RequestParam Integer idInvito) {
        return turistaAutenticatoController.accettaInvito(usernameInvitato, idInvito);
    }


}
