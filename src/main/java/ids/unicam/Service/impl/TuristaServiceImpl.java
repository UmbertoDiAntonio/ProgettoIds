package ids.unicam.Service.impl;

import ids.unicam.Service.TuristaService;
import ids.unicam.models.contenuti.Taggable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class TuristaServiceImpl implements TuristaService {

    private final PoiServiceImpl poiServiceImpl;
    private final NotificaReportService notificaReportService;

    @Autowired
    public TuristaServiceImpl(PoiServiceImpl poiServiceImpl, NotificaReportService notificaReportService) {
        this.poiServiceImpl = poiServiceImpl;
        this.notificaReportService = notificaReportService;
    }

    @Override
    public List<Taggable> findByTag(String tag) {
        return poiServiceImpl.findByTag(tag);
    }

    @Override
    public void report(int idPuntoInteresse, String messaggio) throws IllegalArgumentException {
        notificaReportService.creaNotificaReport(idPuntoInteresse, messaggio);
    }


}
