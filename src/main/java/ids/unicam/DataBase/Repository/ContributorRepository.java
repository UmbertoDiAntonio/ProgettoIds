package ids.unicam.DataBase.Repository;

import ids.unicam.models.Comune;
import ids.unicam.models.attori.Contributor;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ContributorRepository extends JpaRepository<Contributor,String> {


    List<Contributor> findByCognome(String cognome);
    List<Contributor> findByNome(String nome);

    List<Contributor> findByComune(Comune comune);

    @Query("SELECT c.comune FROM Contributor c WHERE c.username = :username")
    Comune findComuneByContributorUsername(String username);
    @Query("SELECT c FROM Contributor c WHERE c.comune.nome = :nome_comune")
    List<Contributor> findByComuneNome(String nome_comune);

}
