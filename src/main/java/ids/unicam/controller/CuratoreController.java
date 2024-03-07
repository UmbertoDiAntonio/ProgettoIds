package ids.unicam.controller;

import ids.unicam.Service.CuratoreService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.DTO.ContributorDTO;
import ids.unicam.models.attori.Ruolo;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.RuoloRegistrazione;
import ids.unicam.models.contenuti.Stato;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/Curatore")
public class CuratoreController{

    private final CuratoreService curatoreService;
    private final GestorePiattaformaService gestorePiattaformaService;

    public CuratoreController(CuratoreService curatoreService, GestorePiattaformaService gestorePiattaformaService) {
        this.curatoreService = curatoreService;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }


    @Operation(summary = "Elenco degli utenti curatore",
            description = "Un elenco degli utenti con ruolo di curatore.")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(curatoreService.getAll());
    }


    @GetMapping("/{username}")
    @Operation(summary = "Curatore dall'username",
            description = "Curatore dall'identificatore univoco username salvato nel database.")
    public ResponseEntity<?> getByUsername(
            @Parameter(description = "username del curatore") @PathVariable String username) {
        return ResponseEntity.ok(curatoreService.getByUsername(username));
    }


    @Operation(summary = "Creazione di un nuovo utente curatore",
            description = "Crea un nuovo utente curatore.")
    public ResponseEntity<?> create(@RequestBody ContributorDTO contributorDTO) {
        try {
            TuristaAutenticato curatore = gestorePiattaformaService.registra(contributorDTO, RuoloRegistrazione.CONTRIBUTOR);
            gestorePiattaformaService.cambiaRuolo(curatore.getUsername(),Ruolo.CURATORE);
            return new ResponseEntity<>(curatore, HttpStatus.OK);
        } catch (ConnessioneFallitaException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }


    @DeleteMapping("/{username}")
    @Operation(summary = "Elimina utente curatore",
            description = "Eliminazione di un utente con ruolo di curatore dall'username univoco.")
    public ResponseEntity<?> delete(
            @Parameter(description = "username del curatore") @PathVariable String username) {
        curatoreService.deleteByUsername(username);
        return ResponseEntity.ok("Utente: '" + username + "' eliminato");
    }

    @DeleteMapping("/eliminaItinerario")
    @Operation(summary = "Elimina un itinerario",
            description = "Eliminazione di un itinerario dall'id univoco.")
    public ResponseEntity<?> eliminaItinerario(
            @Parameter(description = "username del curatore") @RequestParam String usernameCuratore,
            @Parameter(description = "id dell'itinerario da eliminare") @RequestParam Integer idItinerario) {
        try {
            curatoreService.eliminaItinerario(usernameCuratore, idItinerario);
            return ResponseEntity.ok("L'itinerario con id '" + idItinerario + "' e' stato eliminato da utente con username '" + usernameCuratore + "' .");
        } catch (FuoriComuneException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @DeleteMapping("/eliminaPuntoIntesse")
    @Operation(summary = "Elimina un punto di interesse",
            description = "Eliminazione di un punto di interesse dall'id univoco.")
    public ResponseEntity<?> eliminaPuntoInteresse(
            @Parameter(description = "username del curatore") @RequestParam String usernameCuratore,
            @Parameter(description = "id del punto di interesse da eliminare") @RequestParam Integer idPuntoInteresse) {
        try {
            curatoreService.eliminaPuntoInteresse(usernameCuratore, idPuntoInteresse);
            return ResponseEntity.ok("Il punto di interesse con id '" + idPuntoInteresse + "' e' stato eliminato da utente con username '" + usernameCuratore + "' .");
        } catch (FuoriComuneException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @DeleteMapping("/eliminaContest")
    @Operation(summary = "Elimina un contest",
            description = "Eliminazione di un contest dall'id univoco.")
    public ResponseEntity<?> eliminaContest(
            @Parameter(description = "username del curatore") @RequestParam String usernameCuratore,
            @Parameter(description = "id del contest da eliminare") @RequestParam Integer idContest) {
        try {
            curatoreService.eliminaContest(usernameCuratore, idContest);
            return ResponseEntity.ok("Il contest con id '" + idContest + "' e' stato eliminato da utente con username '" + usernameCuratore + "' .");
        } catch (FuoriComuneException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @DeleteMapping("/eliminaMateriale")
    @Operation(summary = "Elimina un Materiale",
            description = "Eliminazione di un Materiale dall'ID.")
    public ResponseEntity<?> eliminaMateriale(
            @Parameter(description = "Username del curatore") @RequestParam String usernameCuratore,
            @Parameter(description = "id del materiale da eliminare") @RequestParam Integer idMateriale) {
        try {
            curatoreService.eliminaMateriale(usernameCuratore, idMateriale);
            return ResponseEntity.ok("Il materiale con id '" + idMateriale + "' e' stato eliminato da utente con username '" + usernameCuratore + "' .");
        } catch (FuoriComuneException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/valutaPuntoInteresse")
    @Operation(summary = "Valutazione di un Punto di Interesse",
            description = "Cambio dello stato di un punto di interesse dall'id univoco.")
    public ResponseEntity<?> valutaPuntoInteresse(
            @Parameter(description = "username del curatore") @RequestParam String usernameCuratore,
            @Parameter(description = "id del punto di interesse da valutare") @RequestParam Integer idPunto,
            @Parameter(description = "approvazione o non del punto di interesse") @RequestParam Stato stato) {
        try {
            return ResponseEntity.ok(curatoreService.valutaPuntoInteresse(usernameCuratore, idPunto, stato.asBoolean()));
        } catch (FuoriComuneException | IllegalArgumentException | UnsupportedOperationException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/valutaMateriale")
    @Operation(summary = "Valutazione di un Materiale",
            description = "Cambio dello stato di un materiale caricato dall'id univoco.")
    public ResponseEntity<?> valutaMateriale(
            @Parameter(description = "username del curatore") @RequestParam String usernameCuratore,
            @Parameter(description = "id del materiale da valutare") @RequestParam Integer idMateriale,
            @Parameter(description = "approvazione o non del materiale") @RequestParam Stato stato) {
        try {
            return ResponseEntity.ok(curatoreService.valutaMateriale(usernameCuratore, idMateriale, stato.asBoolean()));
        } catch (FuoriComuneException | IllegalArgumentException | UnsupportedOperationException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }


    @GetMapping("/getNotifiche/{usernameCuratore}")
    @Operation(summary = "Elenco delle notifiche di un utente Curatore",
            description = "Elenco delle notifiche di un utente Curatore dall'identificatore username univoco.")
    public ResponseEntity<?> getNotifiche(
            @Parameter(description = "username del curatore") @PathVariable String usernameCuratore) {
        try {
            return ResponseEntity.ok(curatoreService.getNotifiche(usernameCuratore));
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

}
