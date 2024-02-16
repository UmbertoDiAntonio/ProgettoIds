package ids.unicam.DataBase.Repository;

import ids.unicam.models.Taggable;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.models.contenuti.Tag;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PoiRepository extends JpaRepository<PuntoInteresse,Integer> {

    List<Taggable> findByTagsValoreContaining(String tag);

    @Query("select p.tags from PuntoInteresse p where p.id=:idPunto")
    List<Tag> getTags(int idPunto);


}
