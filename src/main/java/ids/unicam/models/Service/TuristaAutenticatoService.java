package ids.unicam.models.Service;

import ids.unicam.models.Repository.TuristaAutenticatoRepository;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Foto;
import ids.unicam.models.contenuti.PuntoInteresse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class TuristaAutenticatoService  {
    private final TuristaAutenticatoRepository repository;
    private final MaterialeService materialeService;

    @Autowired
    public TuristaAutenticatoService(TuristaAutenticatoRepository repository, MaterialeService materialeService) {
        this.repository = repository;
        this.materialeService = materialeService;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }


    public TuristaAutenticato save(TuristaAutenticato turistaAutenticato) {
        turistaAutenticato = repository.save(turistaAutenticato);
        return turistaAutenticato;
    }

    public void aggiungiFoto(PuntoInteresse puntoInteresse, Foto foto) {
        materialeService.aggiungiMateriale(puntoInteresse,foto);
    }


    public Optional<TuristaAutenticato> findById(int id) {
        return repository.findById(id);
    }


    public List<TuristaAutenticato> findAll() {
        return repository.findAll();
    }

    public TuristaAutenticato getLast() {
        return repository.findAll().getLast();
    }

    public TuristaAutenticato getFirst() {
        return repository.findAll().getFirst();
    }


    public void deleteAll() {
        repository.deleteAll();
    }


}
