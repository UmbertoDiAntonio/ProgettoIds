package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.MaterialeRepository;
import ids.unicam.Service.MaterialeService;
import ids.unicam.models.DTO.MaterialeDTO;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class MaterialeServiceImpl implements MaterialeService {

    private final MaterialeRepository repository;
    private final TuristaAutenticatoServiceImpl turistaAutenticatoService;


    @Autowired
    public MaterialeServiceImpl(MaterialeRepository repository, TuristaAutenticatoServiceImpl turistaAutenticatoService) {
        this.repository = repository;
        this.turistaAutenticatoService = turistaAutenticatoService;
    }

    @Override
    public void deleteById(int id) {
        repository.deleteById(id);
    }

    @Override
    public Stato getStato(MaterialeGenerico foto) {
        return repository.getStatoById(foto.getId());
    }

    @Override
    public MaterialeGenerico crea(String fileMateriale, TipologiaMateriale tipologiaMateriale, String usernameCreatore) {
        Optional<TuristaAutenticato> oTurista = turistaAutenticatoService.getById(usernameCreatore);
        if(oTurista.isEmpty()){
            logger.error("username non valido");
            throw new IllegalArgumentException("username non valido");
        }
        TuristaAutenticato creatore=oTurista.get();
        return switch (tipologiaMateriale){
            case FOTO -> new Foto(new MaterialeDTO(fileMateriale,creatore));
            case VIDEO -> new Video(new MaterialeDTO(fileMateriale,creatore));
            case TESTO -> new Testo(new MaterialeDTO(fileMateriale,creatore));
            case AUDIO -> new Audio(new MaterialeDTO(fileMateriale,creatore));
        };

    }

    @Override
    public Optional<MaterialeGenerico> getById(int id) {
        return repository.findById(id);
    }

    @Override
    public List<MaterialeGenerico> getAll() {
        return repository.findAll();
    }



    public MaterialeGenerico save(MaterialeGenerico materialeGenerico) {
        return repository.save(materialeGenerico);
    }

    public void deleteAll() {
        repository.deleteAll();
    }

    @Override
    public void approvaMateriale(MaterialeGenerico materialeGenerico, Stato stato) {
        materialeGenerico.setStato(stato);
        save(materialeGenerico);
    }
}
