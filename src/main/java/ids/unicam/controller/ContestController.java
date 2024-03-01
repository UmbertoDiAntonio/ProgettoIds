package ids.unicam.controller;


import ids.unicam.Service.AnimatoreService;
import ids.unicam.Service.ContestService;
import ids.unicam.exception.ContestException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.attori.Animatore;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
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
            @Parameter(description = "accessibilita' del contest") @RequestParam boolean open) {
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

    @PutMapping("/invita/{idContest}")
    @Operation(summary = "Invita utente al contest",
            description = "Invita un utente al contest.")
    public ResponseEntity<?> invita(
            @Parameter(description = "username dell'utente animatore") @RequestParam String usernameAnimatore,
            @Parameter(description = "id del contest") @PathVariable Integer idContest,
            @Parameter(description = "username dell'utente da invitare") @RequestParam String usernameInvitato) {
        try {
            return ResponseEntity.ok(animatoreService.invitaContest(usernameAnimatore, idContest, usernameInvitato));
        } catch (ContestException | IllegalStateException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/contest/termina/{idContest}")
    @Operation(summary = "Termina il Contest",
            description = "Imposta il vincitore e termina il Contest.")
    public ResponseEntity<?> termina(
            @Parameter(description = "Username dell'animatore") @RequestParam String usernameAnimatore,
            @Parameter(description = "id del Contest") @PathVariable Integer idContest,
            @Parameter(description = "id del Materiale Vincitore") @RequestParam Integer idMateriale) {
        try {
            animatoreService.terminaContest(usernameAnimatore, idContest);
            return ResponseEntity.ok("Contest Terminato con vincitore " + idMateriale);
        } catch (UnsupportedOperationException | IllegalArgumentException | ContestException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/contest/setVincitore/{idMateriale}")
    @Operation(summary = "Dichiare il Vincitore",
            description = "Imposta il materiale vincitore del Contest.")
    public ResponseEntity<?> setVincitore(
            @Parameter(description = "Username dell'animatore") @RequestParam String usernameAnimatore,
            @Parameter(description = "id del Contest") @RequestParam Integer idContest,
            @Parameter(description = "id del Materiale Vincitore") @PathVariable Integer idMateriale) {
        try {
            animatoreService.setVincitoreContest(usernameAnimatore, idContest, idMateriale);
            return ResponseEntity.ok("Contest Terminato con vincitore " + idMateriale);
        } catch (UnsupportedOperationException | IllegalArgumentException | ContestException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }


    @PutMapping("/annullaInvito/{idInvito}")
    @Operation(summary = "Annulla un invito",
            description = "Annulla la validità di un invito.")
    public ResponseEntity<?> annullaInvito(
            @Parameter(description = "username dell'animatore") @RequestParam String usernameAnimatore,
            @Parameter(description = "id del invito da annullare") @PathVariable Integer idInvito) {
        try {
            animatoreService.annullaInvito(usernameAnimatore, idInvito);
            return ResponseEntity.ok("Invito con id: '" + idInvito + "' annullato dall'utente con username: '" + usernameAnimatore + "' .");
        } catch (ContestException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }
    @PutMapping("/setDataFine/{data}")
    @Operation(summary = "Imposta la data di Fine Contest",
            description = "Imposta la data di fine Contest.")
    public ResponseEntity<?> setFineContest(
            @Parameter(description = "id del contest") @RequestBody Integer idContest,
            @Parameter(description = "Data di scadenza nel formato YYYY-MM-DD") @PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)  LocalDate data) {
        try {
            contestService.setFineContest(idContest, data);
            return ResponseEntity.ok("Data fine contest "+idContest+" impostata a "+data);
        } catch (FuoriComuneException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }
}
