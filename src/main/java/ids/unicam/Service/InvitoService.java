package ids.unicam.Service;

import ids.unicam.models.DTO.RichiestaCreazioneInvitoDTO;
import ids.unicam.models.DTO.RichiestaCreazioneTuristaDTO;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.TuristaAutenticato;

import java.util.List;

public interface InvitoService {

    void accettaInvito(RichiestaCreazioneTuristaDTO turistaDTO, RichiestaCreazioneInvitoDTO invitoDTO);


    boolean isValid(RichiestaCreazioneInvitoDTO invitoDTO) ;

    List<Invito> getInvitiRicevuti(RichiestaCreazioneTuristaDTO turistaDTO) ;
}
