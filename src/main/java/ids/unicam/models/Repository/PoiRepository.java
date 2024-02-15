package ids.unicam.models.Repository;

import ids.unicam.models.contenuti.PuntoInteresse;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PoiRepository extends JpaRepository<PuntoInteresse,Integer> {

    List<PuntoInteresse> findByTagsContaining(String tag);

    @Query("select p.tags from PuntoInteresse p where p.id=:idPunto")
    List<String> getTags(int idPunto);
}
