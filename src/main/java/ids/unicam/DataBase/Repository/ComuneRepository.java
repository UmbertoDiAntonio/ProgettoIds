package ids.unicam.DataBase.Repository;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.Contest;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface ComuneRepository extends JpaRepository<Comune, String> {
    Optional<Comune> findComuneByNomeIgnoreCase(String nomeComune);
}
