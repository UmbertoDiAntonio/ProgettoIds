package ids.unicam.Service.impl;

import ids.unicam.Service.TuristaService;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class TuristaServiceImpl implements TuristaService {

    private final PoiServiceImpl poiServiceImpl;
    private final TuristaAutenticatoServiceImpl turistaAutenticatoServiceImpl;
    private final NotificaReportServiceImpl notificaReportService;

    @Autowired
    public TuristaServiceImpl(PoiServiceImpl poiServiceImpl, TuristaAutenticatoServiceImpl turistaAutenticatoServiceImpl, NotificaReportServiceImpl notificaReportService) {
        this.poiServiceImpl = poiServiceImpl;
        this.turistaAutenticatoServiceImpl = turistaAutenticatoServiceImpl;
        this.notificaReportService = notificaReportService;
    }

    @Override
    public List<Taggable> findByTag(Tag tag) {
        return poiServiceImpl.findByTag(tag);
    }

    @Override
    public void report(PuntoInteresse puntoInteresse,String msg){
        notificaReportService.creaNotificaReport(puntoInteresse,msg);
    }

    @Override
    public Optional<TuristaAutenticato> accedi(String username, String password) {
        if (turistaAutenticatoServiceImpl.verificaPassword(password, username))
            return turistaAutenticatoServiceImpl.findTuristaByUsername(username);
        return Optional.empty();
    }


}
