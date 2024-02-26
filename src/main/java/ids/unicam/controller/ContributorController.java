package ids.unicam.controller;

import ids.unicam.Service.ContributorService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.InvitoDTO;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.attori.Ruolo;
import ids.unicam.models.attori.TuristaAutenticato;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/Contributor")
public class ContributorController implements ControllerBase<RichiestaCreazioneContributorDTO, String> {

    private final ContributorService contributorService;
    private final GestorePiattaformaService gestorePiattaformaService;
    private final TuristaAutenticatoController turistaAutenticatoController;

    public ContributorController(ContributorService contributorService, GestorePiattaformaService gestorePiattaformaService, TuristaAutenticatoController turistaAutenticatoController) {
        this.contributorService = contributorService;
        this.gestorePiattaformaService = gestorePiattaformaService;
        this.turistaAutenticatoController = turistaAutenticatoController;
    }

    @Override
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(contributorService.getAll());
    }

    @Override
    @GetMapping("/{username}")
    public ResponseEntity<?> getById(@PathVariable String username) {
        return ResponseEntity.ok(contributorService.getById(username));
    }

    @Override
    public ResponseEntity<?> create(RichiestaCreazioneContributorDTO contributorDTO) {
        try {
            TuristaAutenticato contributor = gestorePiattaformaService.registraContributor(contributorDTO, Ruolo.CONTRIBUTOR);
            return new ResponseEntity<>(contributor, HttpStatus.OK);
        } catch (IllegalArgumentException | ConnessioneFallitaException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @Override
    @DeleteMapping("/{username}")
    public ResponseEntity<?> delete(@PathVariable String username) {
        contributorService.deleteById(username);
        return ResponseEntity.ok("Utente: '"+username+ "' eliminato");
    }


    @PutMapping("/accettaInvito")
    public ResponseEntity<?> accettaInvito(@RequestParam String usernameTurista, @RequestParam Integer idInvito) {
        return turistaAutenticatoController.accettaInvito(usernameTurista, idInvito);
    }


}
