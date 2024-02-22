package ids.unicam.controller;

import ids.unicam.Service.AnimatoreService;
import ids.unicam.Service.MaterialeService;
import ids.unicam.models.DTO.MaterialeDTO;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
    public ResponseEntity<?> update(MaterialeDTO entity, Integer id) {
        return  ResponseEntity.ok(materialeService.update(entity,id));
    }

    @Override
    public ResponseEntity<?> delete(Integer id) {
        materialeService.deleteById(id);
        return ResponseEntity.ok("{}");
    }

    public void approvaMateriale(String usernameAnimatore, Integer idContest, Integer idMateriale, boolean stato){
        animatoreService.approvaMateriale(usernameAnimatore,idMateriale,idMateriale,stato);
    }

}
