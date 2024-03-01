package ids.unicam.DataBase.Repository;

import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface MaterialeRepository extends JpaRepository<MaterialeGenerico, Integer> {


    @Query("select m.stato from MaterialeGenerico m where m.id=:id")
    Optional<Stato> getStatoById(int id);

    List<MaterialeGenerico> findAllByFileIn(List<String> file);

}
