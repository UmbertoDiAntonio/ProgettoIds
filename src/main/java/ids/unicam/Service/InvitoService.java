package ids.unicam.Service;

import ids.unicam.models.DTO.InvitoDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.Invito;

import java.util.List;
import java.util.Optional;

public interface InvitoService {

    void accettaInvito(TuristaAutenticatoDTO turistaDTO, InvitoDTO invitoDTO);

    boolean isValid(InvitoDTO invitoDTO) ;

    List<Invito> getInvitiRicevuti(TuristaAutenticatoDTO turistaDTO) ;

    Optional<Invito> findById(int id);
}
