package ids.unicam.models.Repository;

import ids.unicam.Comune;
import ids.unicam.models.attori.ContributorAutorizzato;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ContributorAutorizzatoRepository extends JpaRepository<ContributorAutorizzato,Integer> {
    List<ContributorAutorizzato> findByCognome(String cognome);
    List<ContributorAutorizzato> findByNome(String nome);
    List<ContributorAutorizzato> findByComune(Comune comune);
    List<ContributorAutorizzato> findByComuneNome(String nome_comune);
}
