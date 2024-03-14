package ids.unicam.Service.impl;

import ids.unicam.Service.ContestService;
import ids.unicam.Service.PoiService;
import ids.unicam.Service.TuristaService;
import ids.unicam.models.contenuti.Taggable;
import org.jetbrains.annotations.NotNull;
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
    public @NotNull List<Taggable> findByTag(@NotNull String tag) {
        List<Taggable> temp = new ArrayList<>();
        temp.addAll(contestService.find(contest -> contest.haveTag(tag)));
        temp.addAll(poiService.find(puntoInteresse -> puntoInteresse.haveTag(tag)));
        return temp;
    }

    /**
     * Segnala un punto di interesse ai curatori del comune in cui è localizzato
     *
     * @param idPuntoInteresse l'id del punto di interesse
     * @param messaggio        il motivo della segnalazione
     * @throws IllegalArgumentException se l'id del punto ti interesse non è valido
     */
    @Override
    public void report(int idPuntoInteresse, @NotNull String messaggio) throws IllegalArgumentException {
        notificaReportService.creaNotificaReport(idPuntoInteresse, messaggio);
    }


}
