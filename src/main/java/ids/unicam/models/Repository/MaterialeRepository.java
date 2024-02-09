package ids.unicam.models.Repository;

import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.MaterialeGenerico;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface MaterialeRepository extends JpaRepository<MaterialeGenerico,Integer> {
}
