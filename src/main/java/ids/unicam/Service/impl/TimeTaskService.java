package ids.unicam.Service.impl;

import ids.unicam.Service.ComuneService;
import ids.unicam.Service.ContestService;
import ids.unicam.Service.PoiService;
import ids.unicam.Service.impl.AnimatoreServiceImpl;
import ids.unicam.Service.impl.ComuneServiceImpl;
import ids.unicam.Service.impl.ContestServiceImpl;
import ids.unicam.Service.impl.PoiServiceImpl;
import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.Contest;

import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.util.Calendar;
import java.util.TimerTask;

@Service
public class TimeTaskService extends TimerTask {
    private final ContestService contestService;
    private final ComuneService comuneService;
    private final PoiService poiService;
    private int ultimoGiorno = -1;

    public TimeTaskService(ContestService contestService, ComuneService comuneService, PoiService poiService) {
        this.contestService = contestService;
        this.comuneService = comuneService;
        this.poiService = poiService;
    }

    @Scheduled(fixedRate = 24 * 60 * 60 * 1000)
    @Override
    public void run() {
        Calendar cal = Calendar.getInstance();
        int giornoCorrente = cal.get(Calendar.DAY_OF_YEAR);

        if (giornoCorrente != ultimoGiorno) {
            for(Comune comune:comuneService.findAll()) {
                for (Contest contest:contestService.getContestByComune(comune))
                    contestService.checkIfIsExpired(contest);
                for(PuntoInteresse puntoInteresse:poiService.getPoiByComune(comune)){
                    poiService.checkIfIsExpired(puntoInteresse);
                }
            }
            ultimoGiorno = giornoCorrente;
        }
    }
}
