package ids.unicam.Service.impl;

import ids.unicam.Service.ContenitoreService;
import ids.unicam.models.contenuti.Contenitore;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class ContenitoreServiceImpl implements ContenitoreService{

    @Override
    public void aggiungiMateriale(Contenitore contenitore, MaterialeGenerico materialeGenerico) {
        contenitore.addMateriale(materialeGenerico);
    }

    @Override
    public void rimuoviMateriale(Contenitore contenitore, MaterialeGenerico materialeGenerico) {
        contenitore.rimuoviMateriale(materialeGenerico);
    }

    @Override
    public List<MaterialeGenerico> getMateriali(Contenitore contenitore) {
        return contenitore.getMateriali();
    }
}
