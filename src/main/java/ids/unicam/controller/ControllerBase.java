package ids.unicam.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

public interface ControllerBase<E, ID>{

    @GetMapping("/getAll")
    ResponseEntity<?> getAll();


    @GetMapping("/{id}")
    ResponseEntity<?> getById(@PathVariable ID id);

    @PostMapping
    ResponseEntity<?> create(@RequestBody E entity);

    @PutMapping("/{id}")
    ResponseEntity<?> update(@RequestBody E entity, @PathVariable ID id);

    @DeleteMapping("/{id}")
    ResponseEntity<?> delete(@PathVariable ID id);
}
