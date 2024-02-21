package ids.unicam.controller;


import ids.unicam.Service.ContestService;
import ids.unicam.models.DTO.RichiestaCreazioneContestDTO;
import ids.unicam.models.contenuti.Contest;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/contest")
public class ContestController implements ControllerBase<RichiestaCreazioneContestDTO, Integer>{

    private final ContestService contestService;
    public ContestController(ContestService contestService) {
        this.contestService = contestService;
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

}
