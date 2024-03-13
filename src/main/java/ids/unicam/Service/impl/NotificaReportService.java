package ids.unicam.Service.impl;

import ids.unicam.Service.ComuneService;
import ids.unicam.Service.CuratoreService;
import ids.unicam.Service.PoiService;
import ids.unicam.models.contenuti.notifiche.NotificaBuilder;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.springframework.stereotype.Service;

import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class NotificaReportService {
    private final ComuneService comuneService;
    private final NotificaService notificaService;
    private final PoiService poiService;

    public NotificaReportService(ComuneService comuneService, NotificaService notificaService, PoiService poiService) {
        this.comuneService = comuneService;
        this.notificaService = notificaService;
        this.poiService = poiService;
    }

    public void creaNotificaReport(int idPuntoInteresse, String messaggio) throws IllegalArgumentException {
        Optional<PuntoInteresse> oPuntoInteresse = poiService.getById(idPuntoInteresse);
        if (oPuntoInteresse.isPresent()) {
            PuntoInteresse puntoInteresse = oPuntoInteresse.get();
            comuneService.getCuratoriDelComune(puntoInteresse.getComune().getNome(),"admin").forEach(curatore ->
                    notificaService.save(new NotificaBuilder().withTitolo("Segnalazione: " + puntoInteresse.getNome())
                            .withDescrizione(messaggio)
                            .withDestinatario(curatore).build()));
        } else {
            logger.error("L'ID punto di interesse non e' valido");
            throw new IllegalArgumentException("L'ID punto di interesse non e' valido");
        }
    }
}
