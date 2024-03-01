package ids.unicam.Service.impl;

import ids.unicam.Service.CuratoreService;
import ids.unicam.Service.PoiService;
import ids.unicam.models.contenuti.notifiche.NotificaBuilder;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.springframework.stereotype.Service;

import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class NotificaReportService {
    private final CuratoreService curatoreService;
    private final NotificaService notificaService;
    private final PoiService poiService;

    public NotificaReportService(CuratoreService curatoreService, NotificaService notificaService, PoiService poiService) {
        this.curatoreService = curatoreService;
        this.notificaService = notificaService;
        this.poiService = poiService;
    }

    public void creaNotificaReport(int idPuntoInteresse, String messaggio) throws IllegalArgumentException {
        Optional<PuntoInteresse> oPuntoInteresse = poiService.getById(idPuntoInteresse);
        if (oPuntoInteresse.isPresent()) {
            PuntoInteresse puntoInteresse = oPuntoInteresse.get();
            curatoreService.findByNomeComune(puntoInteresse.getCreatore().getComune().getNome()).forEach(curatore ->
                    notificaService.save(new NotificaBuilder().withTitolo("Segnalazione: " + puntoInteresse.getNome())
                            .withDescrizione(messaggio)
                            .withDestinatario(curatore).build()));
        } else {
            logger.error("L'ID punto di interesse non e' valido");
            throw new IllegalArgumentException("L'ID punto di interesse non e' valido");
        }
    }
}
