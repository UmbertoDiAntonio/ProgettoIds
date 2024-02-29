package ids.unicam.Service;

import ids.unicam.models.attori.Contributor;

import java.util.List;
import java.util.Optional;

public interface ContributorService {
    List<Contributor> getAll();

    void deleteByUsername(String username);

    Optional<Contributor> getByUsername(String username);

}
