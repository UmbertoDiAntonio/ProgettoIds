package ids.unicam.Service;

import ids.unicam.models.attori.ContributorAutorizzato;

import java.util.List;
import java.util.Optional;

public interface ContributorAutorizzatoService {
    List<ContributorAutorizzato> getAll();

    void deleteById(String username);

    Optional<ContributorAutorizzato> getById(String username);

}
