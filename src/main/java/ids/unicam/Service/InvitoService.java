package ids.unicam.Service;

import ids.unicam.models.Invito;
import ids.unicam.models.attori.TuristaAutenticato;

import java.util.List;

public interface InvitoService {

    void accettaInvito(TuristaAutenticato turistaAutenticato, Invito invito);


    boolean isValid(Invito invito) ;

    List<Invito> getInvitiRicevuti(TuristaAutenticato turistaAutenticato) ;
}
