package ids.unicam.controller;

import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.Service.PoiService;
import ids.unicam.Service.TuristaAutenticatoService;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.exception.ContestException;
import ids.unicam.models.DTO.ContributorDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.contenuti.RuoloRegistrazione;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import jakarta.validation.constraints.Min;
import org.jetbrains.annotations.NotNull;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/TuristaAutenticato")
public class TuristaAutenticatoController {

    private final TuristaAutenticatoService turistaAutenticatoService;
    private final GestorePiattaformaService gestorePiattaformaService;
    private final PoiService poiService;

    public TuristaAutenticatoController(TuristaAutenticatoService turistaAutenticatoService, GestorePiattaformaService gestorePiattaformaService, PoiService poiService) {
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.gestorePiattaformaService = gestorePiattaformaService;
        this.poiService = poiService;
    }


    @GetMapping("/getAll")
    @Operation(summary = "Ottieni tutti i Turisti",
            description = "Ottieni Tutti i turisti e superiori.")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(turistaAutenticatoService.getAll());
    }


    @GetMapping("/{username}")
    @Operation(summary = "Ottieni Turista",
            description = "Ottieni un Turista dal suo Username.")
    public ResponseEntity<?> getByUsername(
            @Parameter(description = "username del turista") @PathVariable @NotNull String username) {
        return ResponseEntity.ok(turistaAutenticatoService.getByUsername(username));
    }


    @PostMapping("/crea")
    @Operation(summary = "Crea un utente Turista",
            description = "Crea un utente con il ruolo di turista.")
    public ResponseEntity<?> create(
            @RequestBody TuristaAutenticatoDTO turistaDTO) {
        try {
            return ResponseEntity.ok(gestorePiattaformaService.registra(new ContributorDTO(null, turistaDTO), RuoloRegistrazione.TURISTA));
        } catch (IllegalArgumentException | ConnessioneFallitaException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }


    @DeleteMapping("/{username}")
    @Operation(summary = "Elimina un Turista",
            description = "Eliminazione di un Turista dall'username.")
    public ResponseEntity<?> delete(
            @Parameter(description = "username del turista") @PathVariable String username) {
        turistaAutenticatoService.deleteByUsername(username);
        return ResponseEntity.ok("Utente: '" + username + "' eliminato");
    }


    @PutMapping("/accettaInvito")
    @Operation(summary = "Accetta Invito a Contest",
            description = "Accetta l'invito a partecipare a un contest da parte di un Animatore.")
    public ResponseEntity<?> accettaInvito(
            @Parameter(description = "username del turista") @RequestParam @NotNull String usernameTurista,
            @Parameter(description = "id dell'invito") @RequestParam @Min(0) int idInvito) {
        try {
            turistaAutenticatoService.accettaInvitoContest(usernameTurista, idInvito);
            return ResponseEntity.ok("Il turista con id '" + usernameTurista + "' ha accettato l'invito con id '" + idInvito + "' .");
        } catch (IllegalArgumentException | ContestException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/aggiungiPuntoPreferito")
    @Operation(summary = "Aggiungi Preferito",
            description = "Aggiungi un Punto di Interesse alla tua lista dei preferiti, se esiste.")
    public ResponseEntity<?> aggiungiPreferito(
            @Parameter(description = "username del turista") @RequestParam @NotNull String usernameTurista,
            @Parameter(description = "id del Punto di Interesse") @RequestParam @Min(0) int idPunto) {
        try {
            turistaAutenticatoService.aggiungiPreferito(usernameTurista, idPunto);
            return ResponseEntity.ok("L'utente con username '" + usernameTurista + "' ha aggiunto ai suoi preferiti il punto di interesse con id '" + idPunto + "' .");
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }

    @GetMapping("/getPreferiti/{usernameTurista}")
    @Operation(summary = "Ottieni Preferiti",
            description = "Ottieni la lista di Punti di Interesse preferiti.")
    public ResponseEntity<?> getPreferiti(
            @Parameter(description = "username del turista") @PathVariable @NotNull String usernameTurista) {
        try {
            return ResponseEntity.ok(poiService.getAsList(turistaAutenticatoService.findPreferiti(usernameTurista)));
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }

    @DeleteMapping("/rimuoviPreferito")
    @Operation(summary = "Rimuovi Preferito",
            description = "Rimuovi un Punto di Interesse dalla tua lista dei preferiti, se presente.")
    public ResponseEntity<?> rimuoviPreferito(
            @Parameter(description = "username del turista") @RequestParam @NotNull String usernameTurista,
            @Parameter(description = "id del Punto di Interesse") @RequestParam @Min(0) int idPunto) {
        try {
            turistaAutenticatoService.rimuoviPreferito(usernameTurista, idPunto);
            return ResponseEntity.ok("L'utente con username '" + usernameTurista + "' ha eliminato dai suoi preferiti il punto di interesse con id '" + idPunto + "' .");
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }

    @DeleteMapping("/eliminaNotifiche")
    @Operation(summary = "Elimina Notifiche",
            description = "Elimina tutte le notifiche ricevute, se presenti.")
    public ResponseEntity<?> eliminaNotifiche(
            @Parameter(description = "username del turista") @RequestParam @NotNull String usernameTurista) {
        try {
            turistaAutenticatoService.deleteNotificheByUsername(usernameTurista);
            return ResponseEntity.ok("Notifiche rimosse dall'utente con username '" + usernameTurista + "' .");
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }

    @PutMapping("/partecipaContest")
    @Operation(summary = "Partecipa a un Contest",
            description = "Partecipa a un Contest se Aperto.")
    public ResponseEntity<?> partecipaAlContest(
            @Parameter(description = "id del Contest") @RequestParam @Min(0) int idContest,
            @Parameter(description = "Username del Turista") @RequestParam @NotNull String usernameTurista) {
        try {
            turistaAutenticatoService.partecipaAlContest(idContest, usernameTurista);
            return ResponseEntity.ok("L'utente con username '" + usernameTurista + "' ha iniziato a partecipare al contest con id '" + idContest + "' .");
        } catch (IllegalArgumentException | UnsupportedOperationException | ContestException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }

    @PutMapping("/cancellaIscrizioneContest")
    @Operation(summary = "Annulla iscrizione a un Contest",
            description = "Annulla iscrizione a un Contest.")
    public ResponseEntity<?> cancellaIscrizioneContest(
            @Parameter(description = "id del Contest") @RequestParam @Min(0) int idContest,
            @Parameter(description = "Username del Turista") @RequestParam @NotNull String usernameTurista) {
        try {
            turistaAutenticatoService.cancellaPartecipazioneContest(idContest, usernameTurista);
            return ResponseEntity.ok("L'utente con username '" + usernameTurista + "' ha smesso di partecipare al contest con id '" + idContest + "' .");
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }

    @GetMapping("/getAllInviti/{usernameTurista}")
    @Operation(summary = "Ottieni i tuoi inviti ai contest",
            description = "Ottieni i tuoi inviti ai contest.")
    public ResponseEntity<?> getInviti(
            @Parameter(description = "Username del turista") @PathVariable @NotNull String usernameTurista) {
        try {
            return ResponseEntity.ok(turistaAutenticatoService.getInviti(usernameTurista));
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }

    @GetMapping("/notifiche/{username}")
    @Operation(summary = "Ottieni tutte le Notifiche",
            description = "Ottieni tutte le tue Notifiche .")
    public ResponseEntity<?> getNotifiche(
            @Parameter(description = "Username del turista") @PathVariable @NotNull String username) {
        try {
            return ResponseEntity.ok(turistaAutenticatoService.visualizzaNotifiche(username));
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }


}
