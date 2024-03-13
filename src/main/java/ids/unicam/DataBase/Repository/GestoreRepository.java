package ids.unicam.DataBase.Repository;

import ids.unicam.models.attori.GestorePiattaforma;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface GestoreRepository extends JpaRepository<GestorePiattaforma, String> {

}
