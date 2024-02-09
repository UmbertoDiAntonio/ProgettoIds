package ids.unicam.models.Repository;

import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.models.contenuti.PuntoInteresse;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface MaterialeRepository extends JpaRepository<MaterialeGenerico,Integer> {

}
