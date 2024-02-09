package ids.unicam.models.Repository;

import ids.unicam.Comune;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface ComuneRepository extends JpaRepository<Comune,Integer> {


    Optional<Comune> findByNome(String nomeComune);
}
