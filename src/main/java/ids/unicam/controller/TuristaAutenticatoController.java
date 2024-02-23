package ids.unicam.controller;

import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.Service.TuristaAutenticatoService;
import ids.unicam.models.DTO.InvitoDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.contenuti.notifiche.Notifica;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/TuristaAutenticato")
public class TuristaAutenticatoController implements ControllerBase<TuristaAutenticatoDTO, String> {

    private final TuristaAutenticatoService turistaAutenticatoService;
    private final GestorePiattaformaService gestorePiattaformaService;

    public TuristaAutenticatoController(TuristaAutenticatoService turistaAutenticatoService, GestorePiattaformaService gestorePiattaformaService) {
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }


    @Override
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(turistaAutenticatoService.getAll());
    }


    @Override
    @GetMapping("/{username}")
    public ResponseEntity<?> getById(@PathVariable String username) {
        return ResponseEntity.ok(turistaAutenticatoService.getById(username));
    }

    @Override
    public ResponseEntity<?> create(TuristaAutenticatoDTO turistaDTO) {
        try {
            return ResponseEntity.ok(gestorePiattaformaService.registraTurista(turistaDTO));
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }

    @Override
    public ResponseEntity<?> delete(String username) {
        turistaAutenticatoService.deleteById(username);
        return ResponseEntity.ok("Utente: \'"+username+ "\' eliminato");
    }


    @PutMapping("/accettaInvito")
    public ResponseEntity<?> accettaInvito(@RequestParam TuristaAutenticatoDTO turistaDTO, @RequestParam InvitoDTO invitoDTO) {
        turistaAutenticatoService.accettaInvitoContest(turistaDTO, invitoDTO);
        return ResponseEntity.ok("{}");
    }

    @PutMapping("/aggiungiPuntoPreferito")
    public ResponseEntity<?> aggiungiPreferito(@RequestParam String usernameTurista, @RequestParam Integer idPunto) {
        try {
            turistaAutenticatoService.aggiungiPreferito(usernameTurista, idPunto);
            return ResponseEntity.ok("{}");
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }

    @GetMapping("/getPreferiti/{usernameTurista}")
    public ResponseEntity<?> getPreferiti(@PathVariable String usernameTurista) {
        try {
            return ResponseEntity.ok(turistaAutenticatoService.findPreferiti(usernameTurista));
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }

    @DeleteMapping("/rimuoviPreferito")
    public ResponseEntity<?> rimuoviPreferito(@RequestParam String usernameTurista, @RequestParam Integer idPunto) {
        try {
            turistaAutenticatoService.rimuoviPreferito(usernameTurista, idPunto);
            return ResponseEntity.ok("{}");
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }

    @PutMapping("/partecipaContest")
    public ResponseEntity<?> partecipaAlContest(@RequestParam Integer idContest, @RequestParam String usernameTurista) {
        try {
            turistaAutenticatoService.partecipaAlContest(idContest, usernameTurista);
            return ResponseEntity.ok("{}");
        } catch (IllegalArgumentException | UnsupportedOperationException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }

    @GetMapping("/notifiche/{usernameTurista}")
    public ResponseEntity<?> getNotifiche(@PathVariable String usernameTurista) {
        try {
            return ResponseEntity.ok(turistaAutenticatoService.visualizzaNotifiche(usernameTurista));
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }


}
