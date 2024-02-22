package ids.unicam.DataBase.Repository;

import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface MaterialeRepository extends JpaRepository<MaterialeGenerico,Integer> {


    @Query("select m.stato from MaterialeGenerico m where m.id=:id")
    Stato getStatoById(int id);

}
