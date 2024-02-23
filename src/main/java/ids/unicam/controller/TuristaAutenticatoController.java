package ids.unicam.controller;

import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.Service.TuristaAutenticatoService;
import ids.unicam.models.DTO.InvitoDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
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
    public ResponseEntity<?> getById(String username) {
        return ResponseEntity.ok(turistaAutenticatoService.getById(username));
    }

    @Override
    public ResponseEntity<?> create(TuristaAutenticatoDTO turistaDTO) {
        TuristaAutenticato turista = gestorePiattaformaService.registraTurista(turistaDTO);
        return new ResponseEntity<> (turista, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<?> delete(String username) {
        turistaAutenticatoService.deleteById(username);
        return ResponseEntity.ok("{}");
    }


    @PutMapping("/accettaInvito")
    public void accettaInvito(@RequestParam TuristaAutenticatoDTO turistaDTO, @RequestParam InvitoDTO invitoDTO) {
        turistaAutenticatoService.accettaInvitoContest(turistaDTO, invitoDTO);
    }

    @PutMapping("/aggiungiPreferito")
    public void aggiungiPreferito(@RequestParam String usernameTurista, @RequestParam Integer idPunto) {
        turistaAutenticatoService.aggiungiPreferito(usernameTurista, idPunto);
    }

    @GetMapping("/getPreferiti/{usernameTurista}")
    public List<PuntoInteresse> getPreferiti(@PathVariable String usernameTurista) {
        return turistaAutenticatoService.findPreferiti(usernameTurista);
    }

    @GetMapping("/rimuoviPreferito")
    public void rimuoviPreferito(@RequestParam String usernameTurista, @RequestParam Integer idPunto) {
        turistaAutenticatoService.rimuoviPreferito(usernameTurista,idPunto);

    }

    @PutMapping("/partecipaContest")
    public void partecipaAlContest(@RequestParam Integer idContest, @RequestParam String usernameTurista) {
        turistaAutenticatoService.partecipaAlContest(idContest,usernameTurista);
    }

    @GetMapping("/notifiche/{usernameTurista}")
    public List<Notifica> getNotifiche(@PathVariable String usernameTurista) {
        return turistaAutenticatoService.visualizzaNotifiche(usernameTurista);
    }


}
