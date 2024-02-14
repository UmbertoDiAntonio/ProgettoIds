package ids.unicam.models.Repository;

import ids.unicam.models.contenuti.MaterialeGenerico;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface MaterialeRepository extends JpaRepository<MaterialeGenerico,Integer> {

    List<MaterialeGenerico> findByIdProprietario(int id);
}