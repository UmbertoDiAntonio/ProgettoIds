package ids.unicam.models.Repository;

import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.models.contenuti.PuntoInteresse;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface MaterialeRepository extends JpaRepository<MaterialeGenerico,Integer> {

    List<MaterialeGenerico> findByIdProprietario(int id);

    @Query("select k from MaterialeGenerico p JOIN FETCH PuntoInteresse k where k=:puntoInteresse")
    Optional<PuntoInteresse> findWhereById(@Param("idPunto") PuntoInteresse puntoInteresse);

    @Query("select k from MaterialeGenerico p JOIN FETCH Contest k where k=:contest")
    Optional<Contest> findWhereById(@Param("idPunto") Contest contest);
}
