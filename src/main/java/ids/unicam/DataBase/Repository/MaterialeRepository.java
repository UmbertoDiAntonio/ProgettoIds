package ids.unicam.DataBase.Repository;

import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface MaterialeRepository extends JpaRepository<MaterialeGenerico,Integer> {


}
