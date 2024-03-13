package ids.unicam.controller;


import ids.unicam.Service.AnimatoreService;
import ids.unicam.Service.ContestService;
import ids.unicam.models.attori.Animatore;
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
    @Operation(summary = "Contest dall'identificatore univoco id",
            description = "Contest dall'identificatore univoco id salvato nel database.")
    public ResponseEntity<?> getById(
            @Parameter(description = "id del contest") @PathVariable Integer idContest) {
        return ResponseEntity.ok(contestService.findById(idContest));
    }

    @PostMapping("/crea")
    @Operation(summary = "Creazione di un nuovo contest",
            description = "Crea un nuovo contest.")
    public ResponseEntity<?> create(
            @Parameter(description = "nome del Contest") @RequestParam String nomeContest,
            @Parameter(description = "obiettivo del contest") @RequestParam String obiettivo,
            @Parameter(description = "username del creatore del contest") @RequestParam String usernameCreatore,
            @Parameter(description = "accessibilità del contest") @RequestParam boolean open) {
        Optional<Animatore> oAnimatore = animatoreService.getByUsername(usernameCreatore);
        if (oAnimatore.isEmpty())
            return new ResponseEntity<>("username creatore non valido", HttpStatus.BAD_REQUEST);
        Animatore creatore = oAnimatore.get();
        return ResponseEntity.ok(contestService.creaContest(nomeContest, obiettivo, creatore, open));
    }


    @DeleteMapping("/{idContest}")
    @Operation(summary = "Elimina contest",
            description = "Elimina di un contest dall'id.")
    public ResponseEntity<?> delete(
            @Parameter(description = "id del contest") @PathVariable Integer idContest) {
        contestService.deleteById(idContest);
        return ResponseEntity.ok("Il contest con id '" + idContest + "' e' stato eliminato.");
    }

    @GetMapping("/{nomeComune}/getContests")
    @Operation(summary = "Ottieni i Contest del Comune",
            description = "Ottieni tutti i Contest del Comune.")
    public ResponseEntity<?> getContests(
            @Parameter(description = "nome del comune") @PathVariable String nomeComune) {
        return ResponseEntity.ok(contestService.getContest(contest -> contest.getComune().getNome().equals(nomeComune)));
    }
}
