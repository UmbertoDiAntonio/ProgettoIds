package ids.unicam.controller;


import ids.unicam.Service.AnimatoreService;
import ids.unicam.Service.ContestService;
import ids.unicam.exception.ContestException;
import ids.unicam.models.DTO.RichiestaCreazioneContestDTO;
import ids.unicam.models.Invito;
import ids.unicam.models.contenuti.Contest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/contest")
public class ContestController implements ControllerBase<RichiestaCreazioneContestDTO, Integer>{

    private final ContestService contestService;
    private final AnimatoreService animatoreService;
    public ContestController(ContestService contestService, AnimatoreService animatoreService) {
        this.contestService = contestService;
        this.animatoreService = animatoreService;
    }

    @Override
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(contestService.findAll());
    }

    @Override
    public ResponseEntity<?> getById(Integer id) {
        return ResponseEntity.ok(contestService.findById(id));
    }

    @Override
    public ResponseEntity<?> create(RichiestaCreazioneContestDTO contestDTO) {
        return ResponseEntity.ok(contestService.creaContest(new Contest(contestDTO)));
    }

    @Override
    public ResponseEntity<?> update(RichiestaCreazioneContestDTO comuneDTO, Integer id) {
        return ResponseEntity.ok(contestService.update(new Contest(comuneDTO), id));
    }

    @Override
    public ResponseEntity<?> delete(Integer id) {
        contestService.deleteById(id);
        return ResponseEntity.ok("{}");
    }

    @PutMapping("/invita/{idContest}")
    public ResponseEntity<?> invita(@RequestBody String idAnimatore, @RequestParam Integer idContest, @RequestBody String usernameInvitato){
        try {
            Invito invito = animatoreService.invitaContest(idAnimatore, idContest, usernameInvitato);
            return new ResponseEntity<>(invito, HttpStatus.OK);
        } catch (ContestException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }
}
