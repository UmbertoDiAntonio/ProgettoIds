package ids.unicam.DataBase.Repository;

import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.jetbrains.annotations.NotNull;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Repository
public interface PoiRepository extends JpaRepository<PuntoInteresse, Integer> {

    @Query("select p.tags from PuntoInteresse p where p.id=:idPunto")
    @NotNull List<String> getTags(int idPunto);

    @Query("SELECT p.expireDate FROM PuntoInteresse p WHERE p.id = :id")
    @NotNull Optional<LocalDate> getExpireDateById(int id);

    @Query("select p.stato from PuntoInteresse p where p.id=:idPunto")
    @NotNull Optional<Stato> getStatoById(int idPunto);


    @NotNull Optional<PuntoInteresse> findPuntoInteresseByMaterialiContaining(@NotNull MaterialeGenerico materialeGenerico);

}
