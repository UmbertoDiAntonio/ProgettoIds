package ids.unicam.DataBase.Repository;

import ids.unicam.models.attori.Animatore;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface AnimatoreRepository extends JpaRepository<Animatore, String> {

}
