package ids.unicam.Service.impl;

import ids.unicam.Service.TuristaService;
import ids.unicam.models.DTO.PuntoInteresseDTO;
import ids.unicam.models.contenuti.Taggable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

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
    public List<Taggable> findByTag(String tag) {
        return poiServiceImpl.findByTag(tag);
    }

    @Override
    public void report(PuntoInteresseDTO poiDTO, String messaggio){
        notificaReportService.creaNotificaReport(poiDTO,messaggio);
    }


}
