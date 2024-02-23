package ids.unicam.controller;

import ids.unicam.Service.ContributorService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.attori.Ruolo;
import ids.unicam.models.attori.TuristaAutenticato;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/Contributor")
public class ContributorController implements ControllerBase<RichiestaCreazioneContributorDTO, String> {

    private final ContributorService contributorService;
    private final GestorePiattaformaService gestorePiattaformaService;

    public ContributorController(ContributorService contributorService, GestorePiattaformaService gestorePiattaformaService) {
        this.contributorService = contributorService;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }

    @Override
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(contributorService.getAll());
    }

    @Override
    public ResponseEntity<?> getById(String username) {
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
    public ResponseEntity<?> delete(String username) {
        contributorService.deleteById(username);
        return ResponseEntity.ok("{}");
    }


}
