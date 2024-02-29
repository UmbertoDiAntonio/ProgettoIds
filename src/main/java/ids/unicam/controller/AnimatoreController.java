package ids.unicam.controller;

import ids.unicam.Service.AnimatoreService;
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
@RequestMapping("/Animatore")
public class AnimatoreController implements ControllerBase<RichiestaCreazioneContributorDTO, String> {

    private final AnimatoreService animatoreService;
    private final GestorePiattaformaService gestorePiattaformaService;

    public AnimatoreController(AnimatoreService animatoreService, GestorePiattaformaService gestorePiattaformaService) {
        this.animatoreService = animatoreService;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }

    @Override
    @GetMapping("/getAll")
    @Operation(summary = "Elenco degli utenti animatore",
            description = "Un elenco degli utenti animatore che sono salvati nel database.")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(animatoreService.getAll());
    }

    @Override
    @GetMapping("/{username}")
    @Operation(summary = "Animatore dall'identificatore univoco 'id'",
            description = "Animatore dall'identificatore univoco 'id' salvato nel database.")
    public ResponseEntity<?> getById(
            @Parameter(description = "Username dell'animatore") @PathVariable String username) {
        return ResponseEntity.ok(animatoreService.getById(username));
    }

    @Override
    @PostMapping("/crea")
    @Operation(summary = "Creazione di un nuovo utente animatore",
            description = "Crea un nuovo utente con ruolo di animatore.")
    public ResponseEntity<?> create(@RequestBody RichiestaCreazioneContributorDTO contributorDTO) {
        try {
            return new ResponseEntity<>(gestorePiattaformaService.registraContributor(contributorDTO, Ruolo.ANIMATORE), HttpStatus.OK);
        } catch (ConnessioneFallitaException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @Override
    @DeleteMapping("/{username}")
    @Operation(summary = "Elimina utente Animatore",
            description = "Elimina di un utente con ruolo di Animatore.")
    public ResponseEntity<?> delete(
            @Parameter(description = "username dell'Animatore") @PathVariable String username) {
        animatoreService.deleteById(username);
        return ResponseEntity.ok("Utente: '" + username + "' eliminato");
    }
}
