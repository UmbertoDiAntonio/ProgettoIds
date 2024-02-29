package ids.unicam.Service;

import ids.unicam.models.attori.ContributorAutorizzato;

import java.util.List;
import java.util.Optional;

public interface ContributorAutorizzatoService {
    List<ContributorAutorizzato> getAll();

    void deleteByUsername(String username);

    Optional<ContributorAutorizzato> getByUsername(String username);

}
