package ids.unicam.controller;

import ids.unicam.Service.AnimatoreService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/Animatore")
public class AnimatoreController implements ControllerBase<RichiestaCreazioneContributorDTO, String> {

    private final AnimatoreService animatoreService;
    private final GestorePiattaformaService gestorePiattaformaService;

    public AnimatoreController(AnimatoreService animatoreService, GestorePiattaformaService gestorePiattaformaService) {
        this.animatoreService = animatoreService;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }

    @Override
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(animatoreService.getAll());
    }

    @Override
    public ResponseEntity<?> getById(String username) {
        return ResponseEntity.ok(animatoreService.getById(username));
    }

    @Override
    public ResponseEntity<?> create(RichiestaCreazioneContributorDTO contributorDTO) {
        return ResponseEntity.ok(gestorePiattaformaService.registraContributor(contributorDTO));
    }

    @Override
    public ResponseEntity<?> update(RichiestaCreazioneContributorDTO contributorDTO, String username) {
        return ResponseEntity.ok(animatoreService.update(contributorDTO, username));
    }

    @Override
    public ResponseEntity<?> delete(String username) {
        animatoreService.deleteById(username);
        return ResponseEntity.ok("{}");
    }
}
