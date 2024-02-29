package ids.unicam.DataBase.Repository;

import ids.unicam.models.attori.Contributor;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ContributorRepository extends JpaRepository<Contributor, String> {
    @Query("SELECT c FROM Contributor c WHERE c.comune.nome = :nome_comune")
    List<Contributor> findByComuneNome(String nome_comune);

}
