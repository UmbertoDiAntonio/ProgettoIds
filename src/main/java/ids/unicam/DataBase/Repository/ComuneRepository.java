package ids.unicam.DataBase.Repository;

import ids.unicam.models.Comune;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ComuneRepository extends JpaRepository<Comune, String> {

}
