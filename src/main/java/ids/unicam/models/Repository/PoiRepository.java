package ids.unicam.models.Repository;

import ids.unicam.models.contenuti.PuntoInteresse;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface PoiRepository extends JpaRepository<PuntoInteresse,Integer> {

}
