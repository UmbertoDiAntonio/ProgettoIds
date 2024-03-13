package ids.unicam.DataBase.Repository;

import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Repository
public interface PoiRepository extends JpaRepository<PuntoInteresse, Integer> {

    @Query("select p.tags from PuntoInteresse p where p.id=:idPunto")
    List<String> getTags(int idPunto);

    @Query("SELECT p.expireDate FROM PuntoInteresse p WHERE p.id = :id")
    Optional<LocalDate> getExpireDateById(int id);

    @Query("select p.stato from PuntoInteresse p where p.id=:idPunto")
    Optional<Stato> getStatoById(int idPunto);


    Optional<PuntoInteresse> findPuntoInteresseByMaterialiContaining(MaterialeGenerico materialeGenerico);

    List<PuntoInteresse> findPoiByComuneNome(String nomeComune);
}
