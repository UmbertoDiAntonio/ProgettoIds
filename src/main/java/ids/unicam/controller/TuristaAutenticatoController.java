package ids.unicam.controller;

import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.Service.InvitoService;
import ids.unicam.Service.TuristaAutenticatoService;
import ids.unicam.Service.impl.PoiServiceImpl;
import ids.unicam.models.DTO.InvitoDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;

@RestController
@RequestMapping("/TuristaAutenticato")
public class TuristaAutenticatoController implements ControllerBase<TuristaAutenticatoDTO, String> {

    private final TuristaAutenticatoService turistaAutenticatoService;
    private final GestorePiattaformaService gestorePiattaformaService;
    private final InvitoService invitoService;
    private final PoiServiceImpl poiService;

    public TuristaAutenticatoController(TuristaAutenticatoService turistaAutenticatoService, GestorePiattaformaService gestorePiattaformaService, InvitoService invitoService, PoiServiceImpl poiService) {
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.gestorePiattaformaService = gestorePiattaformaService;
        this.invitoService = invitoService;
        this.poiService = poiService;
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
        return ResponseEntity.ok("Utente: '" + username + "' eliminato");
    }


    @PutMapping("/accettaInvito")
    public ResponseEntity<?> accettaInvito(@RequestParam String usernameTurista, @RequestParam Integer idInvito) {
        Optional<TuristaAutenticato> oTurista = turistaAutenticatoService.getById(usernameTurista);
        if (oTurista.isEmpty()) {
            return new ResponseEntity<>("username turista non valido", HttpStatus.BAD_REQUEST);
        }
        TuristaAutenticato turista = oTurista.get();
        Optional<Invito> oInvito = invitoService.findById(idInvito);
        if (oInvito.isEmpty()) {
            return new ResponseEntity<>("id invito non valido", HttpStatus.BAD_REQUEST);
        }
        Invito invito = oInvito.get();
        try {
            turistaAutenticatoService.accettaInvitoContest(new TuristaAutenticatoDTO(turista), new InvitoDTO(invito.getContest(), invito.getInvitato()));
            return ResponseEntity.ok("Il turista con id '" + usernameTurista + "' ha accettato l'invito con id '" + idInvito + "' .");
        }catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/aggiungiPuntoPreferito")
    public ResponseEntity<?> aggiungiPreferito(@RequestParam String usernameTurista, @RequestParam Integer idPunto) {
        try {
            Optional<PuntoInteresse> oPunto = poiService.getById(idPunto);
            if (oPunto.isEmpty())
                return new ResponseEntity<>("ID punto non valido", HttpStatus.BAD_REQUEST);

            PuntoInteresse puntoInteresse = oPunto.get();
            turistaAutenticatoService.aggiungiPreferito(usernameTurista, puntoInteresse);
            return ResponseEntity.ok("L'utente con id '" + usernameTurista + "' ha aggiunto ai suoi preferiti il punto di interesse con id '" + puntoInteresse.getId() + "' .");
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
            return ResponseEntity.ok("L'utente con id '" + usernameTurista + "' ha eliminato dai suoi preferiti il punto di interesse con id '" + idPunto + "' .");
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }

    @PutMapping("/partecipaContest")
    public ResponseEntity<?> partecipaAlContest(@RequestParam Integer idContest, @RequestParam String usernameTurista) {
        try {
            turistaAutenticatoService.partecipaAlContest(idContest, usernameTurista);
            return ResponseEntity.ok("L'utente con id '" + usernameTurista + "' ha iniziato a partecipare al contest con id '" + idContest + "' .");
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
