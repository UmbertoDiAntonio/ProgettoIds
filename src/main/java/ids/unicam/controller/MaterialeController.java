package ids.unicam.controller;

import ids.unicam.Service.AnimatoreService;
import ids.unicam.Service.MaterialeService;
import ids.unicam.models.DTO.MaterialeDTO;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/materiali")
public class MaterialeController implements ControllerBase<MaterialeDTO,Integer>{
    private final MaterialeService materialeService;
    private final AnimatoreService animatoreService;

    public MaterialeController(MaterialeService materialeService, AnimatoreService animatoreService) {
        this.materialeService = materialeService;
        this.animatoreService = animatoreService;
    }

    @Override
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(materialeService.getAll());
    }

    @Override
    public ResponseEntity<?> getById(Integer id) {
        return ResponseEntity.ok(materialeService.getById(id));
    }

    @Override
    public ResponseEntity<?> create(MaterialeDTO entity) {
        return null;//TODO
    }

    @Override
    public ResponseEntity<?> delete(Integer id) {
        materialeService.deleteById(id);
        return ResponseEntity.ok("{}");
    }

    @PutMapping("/approva/{idMateriale}")
    public ResponseEntity<?> approvaMateriale(@RequestBody String usernameAnimatore, @RequestBody  Integer idContest, @PathVariable Integer idMateriale, @RequestBody  boolean stato){
        try {
            return ResponseEntity.ok(animatoreService.approvaMateriale(usernameAnimatore, idMateriale, idMateriale, stato));
        }catch (UnsupportedOperationException|IllegalArgumentException e){
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

}
