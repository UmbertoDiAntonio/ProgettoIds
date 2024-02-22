package ids.unicam.controller;

import ids.unicam.Service.ContributorService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PutMapping;
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
        return ResponseEntity.ok(gestorePiattaformaService.registraContributor(contributorDTO));
    }

    @Override
    public ResponseEntity<?> update(RichiestaCreazioneContributorDTO contributorDTO, String username) {
        return ResponseEntity.ok(contributorService.update(contributorDTO, username));
    }

    @Override
    public ResponseEntity<?> delete(String username) {
        contributorService.deleteById(username);
        return ResponseEntity.ok("{}");
    }


}
