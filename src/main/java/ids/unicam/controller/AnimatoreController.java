package ids.unicam.controller;

import ids.unicam.Service.AnimatoreService;
import ids.unicam.exception.ContestException;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;

@RestController
@RequestMapping("/Animatore")
public class AnimatoreController {

    private final AnimatoreService animatoreService;

    public AnimatoreController(AnimatoreService animatoreService) {
        this.animatoreService = animatoreService;
    }


    @GetMapping("/getAll")
    @Operation(summary = "Elenco degli utenti animatore",
            description = "Un elenco degli utenti animatore che sono salvati nel database.")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(animatoreService.getAll());
    }


    @GetMapping("/{username}")
    @Operation(summary = "Animatore dall'identificatore univoco id",
            description = "Animatore dall'identificatore univoco id salvato nel database.")
    public ResponseEntity<?> getByUsername(
            @Parameter(description = "Username dell'animatore") @PathVariable String username) {
        return ResponseEntity.ok(animatoreService.getByUsername(username));
    }


    @PutMapping("/approvaMateriale/{idMateriale}")
    @Operation(summary = "Approva un materiale",
            description = "L'utente curatore approva un materiale caricato.")
    public ResponseEntity<?> approvaMateriale(
            @Parameter(description = "username dell'animatore") @RequestBody String usernameAnimatore,
            @Parameter(description = "id del contest") @RequestBody Integer idContest,
            @Parameter(description = "id del materiale da approvare") @PathVariable Integer idMateriale,
            @Parameter(description = "scelta di approvare o non il materiale") @RequestBody boolean stato) {
        try {
            animatoreService.approvaMateriale(usernameAnimatore, idContest, idMateriale, stato);
            return ResponseEntity.ok("stato materiale " + idMateriale + " cambiato in " + stato);
        } catch (UnsupportedOperationException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/setDataFine/{data}")
    @Operation(summary = "Imposta la data di Fine Contest",
            description = "Imposta la data di fine Contest.")
    public ResponseEntity<?> setFineContest(
            @Parameter(description = "id del contest") @RequestBody Integer idContest,
            @Parameter(description = "Data di scadenza nel formato YYYY-MM-DD") @PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate data,
            @Parameter(description = "username dell' animatore") @RequestBody String usernameAnimatore) {
        try {
            animatoreService.setFineContest(idContest, data, usernameAnimatore);
            return ResponseEntity.ok("Data fine contest " + idContest + " impostata a " + data);
        } catch (UnsupportedOperationException | IllegalArgumentException e) {
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
            @Parameter(description = "id del Contest") @PathVariable Integer idContest) {
        try {
            animatoreService.terminaContest(usernameAnimatore, idContest);
            return ResponseEntity.ok("Contest Terminato ");
        } catch (UnsupportedOperationException | IllegalArgumentException | ContestException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/contest/setVincitore/{idMateriale}")
    @Operation(summary = "Dichiara il Vincitore",
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

    @PutMapping("/addTag")
    @Operation(summary = "Aggiunta di un tag",
            description = "Aggiunge un nuovo tag a un Contest.")
    public ResponseEntity<?> aggiungiTag(
            @Parameter(description = "Nome del tag") @RequestParam String nomeTag,
            @Parameter(description = "ID del contest") @RequestParam Integer idContest,
            @Parameter(description = "username dell' Animatore") @RequestParam String usernameAnimatore) {
        try {
            animatoreService.aggiungiTagContest(idContest, nomeTag, usernameAnimatore);
            return ResponseEntity.ok("Aggiunto tag '" + nomeTag + "' al punto di interesse: '" + idContest + "' .");
        } catch (ContestException | IllegalArgumentException | IllegalStateException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/removeTag")
    @Operation(summary = "Rimozione di un tag",
            description = "Rimuove un tag da un Contest.")
    public ResponseEntity<?> rimuoviTag(
            @Parameter(description = "Nome del tag") @RequestParam String nomeTag,
            @Parameter(description = "ID del contest") @RequestParam Integer idContest,
            @Parameter(description = "username dell' Animatore") @RequestParam String usernameAnimatore) {
        try {
            animatoreService.rimuoviTagContest(idContest, nomeTag, usernameAnimatore);
            return ResponseEntity.ok("Rimosso tag '" + nomeTag + "' dal punto di interesse: '" + idContest + "' .");
        } catch (ContestException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }
}
