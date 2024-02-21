package ids.unicam.DataBase.Repository;

import ids.unicam.models.Comune;
import ids.unicam.models.attori.ContributorAutorizzato;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ContributorAutorizzatoRepository extends JpaRepository<ContributorAutorizzato,String> {
    List<ContributorAutorizzato> findByCognome(String cognome);
    List<ContributorAutorizzato> findByNome(String nome);
    List<ContributorAutorizzato> findByComune(Comune comune);
    List<ContributorAutorizzato> findByComuneNome(String nome_comune);
}
