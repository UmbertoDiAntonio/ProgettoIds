package ids.unicam.controller;


import ids.unicam.Service.AnimatoreService;
import ids.unicam.Service.ContestService;
import ids.unicam.exception.ContestException;
import ids.unicam.models.DTO.RichiestaCreazioneContestDTO;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.contenuti.Contest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;

@RestController
@RequestMapping("/contest")
public class ContestController {
    private final ContestService contestService;
    private final AnimatoreService animatoreService;

    public ContestController(ContestService contestService, AnimatoreService animatoreService) {
        this.contestService = contestService;
        this.animatoreService = animatoreService;
    }


    @GetMapping("/getAll")
    @Operation(summary = "Elenco dei contest",
            description = "Un elenco dei contest che sono salvati nel database.")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(contestService.findAll());
    }

    @GetMapping("/{idContest}")
    @Operation(summary = "Contest dall'identificatore univoco 'id'",
            description = "Contest dall'identificatore univoco 'id' salvato nel database.")
    public ResponseEntity<?> getById(
            @Parameter(description = "id del contest") @PathVariable Integer idContest) {
        return ResponseEntity.ok(contestService.findById(idContest));
    }

    @PostMapping("/crea")
    @Operation(summary = "Creazione di un nuovo contest",
            description = "Crea un nuovo contest.")
    public ResponseEntity<?> create(
            @Parameter(description = "nome del Contest")@RequestParam String nomeContest,
            @Parameter(description = "obiettivo del contest")@RequestParam String obiettivo,
            @Parameter(description = "username del creatore del contest")@RequestParam String usernameCreatore,
            @Parameter(description = "accessibilita' del contest") @RequestParam boolean open) {
        Optional<Animatore> oAnimatore = animatoreService.getById(usernameCreatore);
        if(oAnimatore.isEmpty())
            return new ResponseEntity<>("username creatore non valido", HttpStatus.BAD_REQUEST);
        Animatore creatore = oAnimatore.get();
        RichiestaCreazioneContestDTO contestDTO = new RichiestaCreazioneContestDTO(nomeContest, obiettivo, creatore, open);
        return ResponseEntity.ok(contestService.creaContest(new Contest(contestDTO)));
    }


    @DeleteMapping("/{idContest}")
    @Operation(summary = "Elimina contest",
            description = "Elimina di un contest dall'id.")
    public ResponseEntity<?> delete(
            @Parameter(description = "id del contest")@PathVariable Integer idContest) {
        contestService.deleteById(idContest);
        return ResponseEntity.ok("Il contest con id '" + idContest + "' e' stato eliminato.");
    }

    @PutMapping("/invita/{idContest}")
    @Operation(summary = "Invita utente al contest",
            description = "Invita un utente al contest.")
    public ResponseEntity<?> invita(
            @Parameter(description = "id dell'utente animatore")@RequestParam String idAnimatore,
            @Parameter(description = "id del contest")@RequestParam Integer idContest,
            @Parameter(description = "username dell'utente da invitare")@RequestParam String usernameInvitato) {
        try {
            return ResponseEntity.ok(animatoreService.invitaContest(idAnimatore, idContest, usernameInvitato));
        } catch (ContestException | IllegalStateException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/termina/{idContest}")
    @Operation(summary = "Termina il Contest",
            description = "Imposta il vincitore e termina il Contest.")
    public ResponseEntity<?> termina(
            @Parameter(description = "Username dell'animatore") @RequestParam String idAnimatore,
            @Parameter(description = "id del Contest") @RequestParam Integer idContest,
            @Parameter(description = "id del Materiale Vincitore") @RequestParam Integer idMateriale) {
        try {
            animatoreService.terminaContest(idAnimatore, idContest, idMateriale);
            return ResponseEntity.ok("Contest Terminato con vincitore "+idMateriale);
        } catch (ContestException | UnsupportedOperationException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/approvaMateriale/{idMateriale}")
    @Operation(summary = "Approva un materiale",
            description = "L'utente curatore approva un materiale caricato.")
    public ResponseEntity<?> approvaMateriale(
            @Parameter(description = "username dell'animatore")@RequestBody String usernameAnimatore,
            @Parameter(description = "id del contest")@RequestBody Integer idContest,
            @Parameter(description = "id del materiale da approvare")@PathVariable Integer idMateriale,
            @Parameter(description = "scelta di approvare o non il materiale")@RequestBody boolean stato) {
        try {
            return ResponseEntity.ok(animatoreService.approvaMateriale(usernameAnimatore, idContest, idMateriale, stato));
        } catch (UnsupportedOperationException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

}
