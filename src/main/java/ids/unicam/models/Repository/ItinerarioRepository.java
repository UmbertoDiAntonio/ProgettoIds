package ids.unicam.models.Repository;

import ids.unicam.Comune;
import ids.unicam.models.contenuti.Itinerario;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ItinerarioRepository extends JpaRepository<Itinerario,Integer> {
    List<Itinerario> findAllByComune(Comune comune);
}
