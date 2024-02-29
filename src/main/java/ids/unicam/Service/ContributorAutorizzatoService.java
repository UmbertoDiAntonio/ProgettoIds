package ids.unicam.Service;

import ids.unicam.models.attori.ContributorAutorizzato;

import java.util.List;
import java.util.Optional;

public interface ContributorAutorizzatoService {
    ContributorAutorizzato save(ContributorAutorizzato contributorAutorizzato);

    List<ContributorAutorizzato> getAll();

    void deleteByUsername(String username);

    Optional<ContributorAutorizzato> getByUsername(String username);

    List<ContributorAutorizzato> findByNomeComune(String nomeComune);
}
