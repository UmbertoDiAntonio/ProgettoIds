package ids.unicam.Service;

import ids.unicam.models.attori.Contributor;

import java.util.List;
import java.util.Optional;

public interface ContributorService {
    Contributor save(Contributor contributor);

    List<Contributor> getAll();

    List<Contributor> findByNomeComune(String nomeComune);

    void deleteByUsername(String username);

    Optional<Contributor> getByUsername(String username);

}
