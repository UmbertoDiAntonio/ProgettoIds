package ids.unicam.models.Repository;

import ids.unicam.Comune;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.PuntoInteresse;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ItinerarioRepository extends JpaRepository<Itinerario,Integer> {
    List<Itinerario> findAllByComune(Comune comune);

    @Query("SELECT i FROM Itinerario i WHERE i.id = :id")
    Itinerario findTappeByItinerario(@Param("id") int id);
}
