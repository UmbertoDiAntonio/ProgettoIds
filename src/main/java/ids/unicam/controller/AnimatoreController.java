package ids.unicam.controller;

import ids.unicam.Service.AnimatoreService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.attori.Ruolo;
import org.springframework.http.HttpStatus;
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
        try {
            return new ResponseEntity<>(gestorePiattaformaService.registraContributor(contributorDTO, Ruolo.ANIMATORE),HttpStatus.OK);
        } catch (ConnessioneFallitaException  | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @Override
    public ResponseEntity<?> delete(String username) {
        animatoreService.deleteById(username);
        return ResponseEntity.ok("{}");
    }
}
