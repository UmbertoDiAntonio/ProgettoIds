package ids.unicam.DataBase.Repository;

import ids.unicam.models.Comune;
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

    @Query("SELECT Count(i) FROM Itinerario i JOIN  i.percorso Where i.id = :id")
    Integer countNumeroTappeItinerario(@Param("id") int id);

    @Query("SELECT i.percorso FROM Itinerario i  Where i.id = :id")
    List<PuntoInteresse> findTappeByItinerario(@Param("id") int id);
}
