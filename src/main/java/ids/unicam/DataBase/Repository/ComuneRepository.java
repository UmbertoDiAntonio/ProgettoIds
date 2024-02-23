package ids.unicam.DataBase.Repository;

import ids.unicam.models.Comune;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface ComuneRepository extends JpaRepository<Comune,Integer> {


    Optional<Comune> findByNome(String nomeComune);


    void deleteByNome(String nomeComune);
}
