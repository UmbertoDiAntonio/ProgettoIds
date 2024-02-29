package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.MaterialeRepository;
import ids.unicam.Service.MaterialeService;
import ids.unicam.models.DTO.MaterialeDTO;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contenitore;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static ids.unicam.Main.logger;


@Service
public class MaterialeServiceImpl implements MaterialeService {

    private final MaterialeRepository repository;


    @Autowired
    public MaterialeServiceImpl(MaterialeRepository repository) {
        this.repository = repository;
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
    public MaterialeGenerico crea(String fileMateriale, TipologiaMateriale tipologiaMateriale, TuristaAutenticato creatore) throws IllegalArgumentException {

        MaterialeGenerico materialeGenerico = switch (tipologiaMateriale) {
            case FOTO -> new Foto(new MaterialeDTO(fileMateriale, creatore));
            case VIDEO -> new Video(new MaterialeDTO(fileMateriale, creatore));
            case TESTO -> new Testo(new MaterialeDTO(fileMateriale, creatore));
            case AUDIO -> new Audio(new MaterialeDTO(fileMateriale, creatore));
        };
        if (creatore instanceof ContributorAutorizzato)
            materialeGenerico.setStato(Stato.APPROVATO);

        return save(materialeGenerico);
    }

    @Override
    public String getBase64ById(Integer id) {
        Optional<MaterialeGenerico> oMateriale = getById(id);
        if (oMateriale.isEmpty()) {
            logger.error("L'id del materiale non e' valido");
            throw new IllegalArgumentException("L'id del materiale non e' valido");
        }
        return oMateriale.get().getBase64();
    }

    @Override
    public void aggiungiMateriale(Contenitore contenitore, MaterialeGenerico materialeGenerico) {
        contenitore.addMateriale(materialeGenerico);
        save(materialeGenerico);
    }

    @Override
    public Optional<MaterialeGenerico> getById(int id) {
        return repository.findById(id);
    }

    @Override
    public List<MaterialeGenerico> getAll() {
        List<String> file = new ArrayList<>();
        File folder = new File("src/main/resources/materials");
        for (File fileMateriale : Objects.requireNonNull(folder.listFiles())) {
            file.add("./" + fileMateriale.getPath().replace("\\", "/"));
        }
        return repository.findAllByFileIn(file);
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
