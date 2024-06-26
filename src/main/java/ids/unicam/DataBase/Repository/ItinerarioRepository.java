package ids.unicam.DataBase.Repository;

import ids.unicam.models.contenuti.Itinerario;
import org.jetbrains.annotations.NotNull;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface ItinerarioRepository extends JpaRepository<Itinerario, Integer> {
    @NotNull Optional<Itinerario> findByNome(@NotNull String nome);

}
