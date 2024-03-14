package ids.unicam.DataBase.Repository;

import ids.unicam.models.attori.ContributorAutorizzato;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ContributorAutorizzatoRepository extends JpaRepository<ContributorAutorizzato, String> {
}
