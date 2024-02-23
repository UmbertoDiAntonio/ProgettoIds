package ids.unicam.controller;

import ids.unicam.Service.ContributorAutorizzatoService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/ContributorAutorizzato")
public class ContributorAutorizzatoController implements ControllerBase<RichiestaCreazioneContributorDTO, String> {

    private final ContributorAutorizzatoService contributorAutorizzatoService;
    private final GestorePiattaformaService gestorePiattaformaService;

    public ContributorAutorizzatoController(ContributorAutorizzatoService contributorAutorizzatoService, GestorePiattaformaService gestorePiattaformaService) {
        this.contributorAutorizzatoService = contributorAutorizzatoService;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }

    @Override
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(contributorAutorizzatoService.getAll());
    }

    @Override
    public ResponseEntity<?> getById(String username) {
        return ResponseEntity.ok(contributorAutorizzatoService.getById(username));
    }

    @Override
    public ResponseEntity<?> create(RichiestaCreazioneContributorDTO contributorDTO) {
        try {
            return new ResponseEntity<>(gestorePiattaformaService.registraContributor(contributorDTO), HttpStatus.OK);
        } catch (ConnessioneFallitaException | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @Override
    public ResponseEntity<?> delete(String username) {
        contributorAutorizzatoService.deleteById(username);
        return ResponseEntity.ok("{}");
    }
}