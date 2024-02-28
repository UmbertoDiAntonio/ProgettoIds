package ids.unicam.controller;


import ids.unicam.Service.AnimatoreService;
import ids.unicam.Service.ContestService;
import ids.unicam.exception.ContestException;
import ids.unicam.models.DTO.RichiestaCreazioneContestDTO;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.contenuti.Contest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;

@RestController
@RequestMapping("/contest")
public class ContestController{

    private final ContestService contestService;
    private final AnimatoreService animatoreService;
    public ContestController(ContestService contestService, AnimatoreService animatoreService) {
        this.contestService = contestService;
        this.animatoreService = animatoreService;
    }


    @GetMapping("/getAll")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(contestService.findAll());
    }

    @GetMapping("/{idContest}")
    public ResponseEntity<?> getById(@PathVariable Integer idContest) {
        return ResponseEntity.ok(contestService.findById(idContest));
    }


    @PostMapping("/crea")
    public ResponseEntity<?> create(String nomeContest,String obiettivo,String usernameCreatore,boolean open) {
        Optional<Animatore> oAnimatore = animatoreService.getById(usernameCreatore);
        if(oAnimatore.isEmpty())
            return new ResponseEntity<>("username creatore non valido", HttpStatus.BAD_REQUEST);
        Animatore creatore = oAnimatore.get();
        RichiestaCreazioneContestDTO contestDTO = new RichiestaCreazioneContestDTO(nomeContest, obiettivo, creatore, open);
        return ResponseEntity.ok(contestService.creaContest(new Contest(contestDTO)));
    }


    @DeleteMapping("/{idContest}")
    public ResponseEntity<?> delete(@PathVariable Integer idContest) {
        contestService.deleteById(idContest);
        return ResponseEntity.ok("Il contest con id '"+idContest+"' e' stato eliminato.");
    }

    @PutMapping("/invita/{idContest}")
    public ResponseEntity<?> invita(@RequestParam String idAnimatore, @RequestParam Integer idContest, @RequestParam String usernameInvitato){
        try {
            return ResponseEntity.ok( animatoreService.invitaContest(idAnimatore, idContest, usernameInvitato));
        } catch (ContestException |IllegalStateException|IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }
    @PutMapping("/approvaMateriale/{idMateriale}")
    public ResponseEntity<?> approvaMateriale(@RequestBody String usernameAnimatore, @RequestBody  Integer idContest, @PathVariable Integer idMateriale, @RequestBody  boolean stato){
        try {
            return ResponseEntity.ok(animatoreService.approvaMateriale(usernameAnimatore, idContest, idMateriale, stato));
        }catch (UnsupportedOperationException|IllegalArgumentException e){
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    //TODO termina contest
    //TODO setFineContest
}
