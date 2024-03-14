package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.MaterialeRepository;
import ids.unicam.Service.MaterialeService;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
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
    public void deleteById(int id, @NotNull String usernameGestore) {
        repository.deleteById(id);
    }


    @Override
    public @NotNull MaterialeGenerico crea(@Nullable String fileMateriale, @NotNull TipologiaMateriale tipologiaMateriale, @NotNull TuristaAutenticato creatore) throws IllegalArgumentException {
        if (fileMateriale == null) {
            logger.error("Percorso del materiale non trovato");
            throw new IllegalArgumentException("Percorso del materiale non trovato");
        }
        TuristaAutenticatoDTO creatoreDTO = new TuristaAutenticatoDTO(creatore.getNome(), creatore.getCognome(), creatore.getDataNascita(), creatore.getPassword(), creatore.getUsername());
        MaterialeGenerico materialeGenerico = switch (tipologiaMateriale) {
            case FOTO -> new Foto(fileMateriale, creatoreDTO);
            case VIDEO -> new Video(fileMateriale, creatoreDTO);
            case TESTO -> new Testo(fileMateriale, creatoreDTO);
            case AUDIO -> new Audio(fileMateriale, creatoreDTO);
        };
        if (creatore instanceof ContributorAutorizzato)
            materialeGenerico.setStato(Stato.APPROVATO);

        return save(materialeGenerico);
    }

    @Override
    public @NotNull String getBase64ById(int id) {
        Optional<MaterialeGenerico> oMateriale = getById(id);
        if (oMateriale.isEmpty()) {
            logger.warn("L'id del materiale non e' valido");
            throw new IllegalArgumentException("L'id del materiale non e' valido");
        }
        return oMateriale.get().getBase64();
    }


    @Override
    public @NotNull Optional<Stato> getStato(int idMateriale) {
        return repository.getStatoById(idMateriale);
    }

    @Override
    public @NotNull Optional<MaterialeGenerico> getById(int id) {
        return repository.findById(id);
    }

    @Override
    public @NotNull List<MaterialeGenerico> getAll() {
        List<String> file = new ArrayList<>();
        File folder = new File("src/main/resources/materials");
        for (File fileMateriale : Objects.requireNonNull(folder.listFiles())) {
            file.add("./" + fileMateriale.getPath().replace("\\", "/"));
        }
        return repository.findAllByFileIn(file);
    }


    @Override
    public @NotNull MaterialeGenerico save(@NotNull MaterialeGenerico materialeGenerico) {
        return repository.save(materialeGenerico);
    }

}
