package ids.unicam.controller;

import ids.unicam.Service.CuratoreService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.attori.Ruolo;
import ids.unicam.models.contenuti.Stato;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/Curatore")
public class CuratoreController implements ControllerBase<RichiestaCreazioneContributorDTO, String> {

    private final CuratoreService curatoreService;
    private final GestorePiattaformaService gestorePiattaformaService;

    public CuratoreController(CuratoreService curatoreService, GestorePiattaformaService gestorePiattaformaService) {
        this.curatoreService = curatoreService;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }

    @Override
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(curatoreService.getAll());
    }

    @Override
    @GetMapping("/{username}")
    public ResponseEntity<?> getById(@PathVariable String username) {
        return ResponseEntity.ok(curatoreService.getById(username));
    }

    @Override
    public ResponseEntity<?> create(RichiestaCreazioneContributorDTO contributorDTO) {
        try {
            return new ResponseEntity<>(gestorePiattaformaService.registraContributor(contributorDTO, Ruolo.CURATORE), HttpStatus.OK);
        } catch (ConnessioneFallitaException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @Override
    @DeleteMapping("/{username}")
    public ResponseEntity<?> delete(@PathVariable String username) {
        curatoreService.deleteById(username);
        return ResponseEntity.ok("Utente: '" + username + "' eliminato");
    }

    @DeleteMapping("eliminaItinerario")
    public ResponseEntity<?> eliminaItinerario(@RequestParam String usernameCuratore, @RequestParam Integer idItinerario) {
        try {
            curatoreService.eliminaItinerario(usernameCuratore, idItinerario);
            return ResponseEntity.ok("{}");
        } catch (FuoriComuneException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @DeleteMapping("eliminaPuntoIntesse")
    public ResponseEntity<?> eliminaPuntoInteresse(@RequestParam String usernameCuratore, @RequestParam Integer idPuntoInteresse) {
        try {
            curatoreService.eliminaPuntoInteresse(usernameCuratore, idPuntoInteresse);
            return ResponseEntity.ok("{}");
        } catch (FuoriComuneException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @DeleteMapping("eliminaContest")
    public ResponseEntity<?> eliminaContest(@RequestParam String usernameCuratore, @RequestParam Integer idContest) {
        try {
            curatoreService.eliminaContest(usernameCuratore, idContest);
            return ResponseEntity.ok("{}");
        } catch (FuoriComuneException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("valutaPuntoInteresse")
    public ResponseEntity<?> valutaPuntoInteresse(@RequestParam String usernameCuratore, @RequestParam Integer idPunto, @RequestParam Stato stato) {
        try {
            return ResponseEntity.ok(curatoreService.valutaPuntoInteresse(usernameCuratore, idPunto, stato.asBoolean()));
        } catch (FuoriComuneException | IllegalArgumentException | UnsupportedOperationException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("valutaMateriale")
    public ResponseEntity<?> valutaMateriale(@RequestParam String usernameCuratore, @RequestParam Integer idMateriale, @RequestParam Stato stato) {
        try {
            return ResponseEntity.ok(curatoreService.valutaMateriale(usernameCuratore, idMateriale, stato.asBoolean()));
        } catch (FuoriComuneException | IllegalArgumentException | UnsupportedOperationException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("segui/{usernameCuratore}")
    public ResponseEntity<?> subscribeOsservatore(@PathVariable String usernameCuratore, @RequestParam String usernameContributor) {
        try {
            curatoreService.aggiungiOsservatore(usernameCuratore, usernameContributor);
            return ResponseEntity.ok("{}");
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("unsuscribe/{usernameCuratore}")
    public ResponseEntity<?> unsubscibeOsservatore(@PathVariable String usernameCuratore, @RequestParam String usernameContributor) {
        try {
            curatoreService.rimuoviOsservatore(usernameCuratore, usernameContributor);
            return ResponseEntity.ok("{}");

        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @GetMapping("getNotifiche/{usernameCuratore}")
    public ResponseEntity<?> getNotifiche(@PathVariable String usernameCuratore) {
        try {
            return ResponseEntity.ok(curatoreService.getNotifiche(usernameCuratore));
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

}
