package ids.unicam.Service;

import ids.unicam.models.Invito;
import ids.unicam.models.attori.TuristaAutenticato;

import java.util.List;
import java.util.Optional;

public interface InvitoService {

    void accettaInvito(TuristaAutenticato turistaAutenticato, Invito invito);

    boolean isValid(Invito invito) ;

    List<Invito> getInvitiRicevuti(String usernameTurista) ;

    Optional<Invito> findById(int id);
}
