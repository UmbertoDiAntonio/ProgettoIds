package ids.unicam.Service.impl;

import ids.unicam.Service.ContestService;
import ids.unicam.Service.PoiService;
import ids.unicam.Service.TuristaService;
import ids.unicam.models.contenuti.Taggable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class TuristaServiceImpl implements TuristaService {

    private final PoiService poiService;
    private final NotificaReportService notificaReportService;
    private final ContestService contestService;

    @Autowired
    public TuristaServiceImpl(PoiService poiService, NotificaReportService notificaReportService, ContestService contestService) {
        this.poiService = poiService;
        this.notificaReportService = notificaReportService;
        this.contestService = contestService;
    }

    @Override
    public List<Taggable> findByTag(String tag) {
        List<Taggable> temp=new ArrayList<>();
        temp.addAll(contestService.find(contest -> contest.getTags().contains(tag)));
        temp.addAll(poiService.find(puntoInteresse -> puntoInteresse.getTags().contains(tag)));
        return temp;
    }

    @Override
    public void report(int idPuntoInteresse, String messaggio) throws IllegalArgumentException {
        notificaReportService.creaNotificaReport(idPuntoInteresse, messaggio);
    }


}
