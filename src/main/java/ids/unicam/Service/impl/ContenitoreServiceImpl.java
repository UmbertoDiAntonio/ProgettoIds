package ids.unicam.Service.impl;

import ids.unicam.Service.ContenitoreService;
import ids.unicam.models.contenuti.Contenitore;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class ContenitoreServiceImpl implements ContenitoreService{

    private final PoiServiceImpl poiService;
    private final ContestServiceImpl contestService;

    @Autowired
    public ContenitoreServiceImpl(PoiServiceImpl poiService, ContestServiceImpl contestService) {
        this.poiService = poiService;
        this.contestService = contestService;
    }

    @Override
    public Contenitore salva(Contenitore contenitore) {
        if(contenitore instanceof PuntoInteresse puntoInteresse) {
            return poiService.save(puntoInteresse);
        }
        if(contenitore instanceof Contest contest) {
            return contestService.save(contest);
        }
        return null;
    }

    @Override
    public void aggiungiMateriale(Contenitore contenitore, MaterialeGenerico materialeGenerico) {
        contenitore.addMateriale(materialeGenerico);
        salva(contenitore);
    }

    @Override
    public void rimuoviMateriale(Contenitore contenitore, MaterialeGenerico materialeGenerico) {
        contenitore.rimuoviMateriale(materialeGenerico);
        salva(contenitore);

    }

    @Override
    public List<MaterialeGenerico> getMateriali(Contenitore contenitore) {
        return contenitore.getMateriali();
    }
}
